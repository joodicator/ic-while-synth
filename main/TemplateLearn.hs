#!/usr/bin/env runghc

import Clingo hiding (readArgs)
import qualified While

import IterativeLearn hiding (
    main, Conf(..), defaultConf, readConfFacts, readConfFile,
    readArgs, readArgsFlag, interactivePause, showInteractiveHelp)
import qualified IterativeLearn as IL

import Data.List
import Data.Maybe
import Data.Char
import Data.Function
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Applicative
import System.IO
import System.Environment
import System.Exit

--------------------------------------------------------------------------------
type Template
  = [TemplateLine]

data TemplateLine
  = TLPre Condition
  | TLMid Condition
  | TLPost Condition
  | TLInstr Instruction
  deriving Show

--------------------------------------------------------------------------------
data Conf = Conf{
    cfIntRange        :: (Integer, Integer),
    cfTimeMax         :: Integer, 
    cfLineLimitMax    :: Maybe Integer,
    cfConstants       :: [Value],   
    cfProgramVars     :: [Variable],
    cfLogicVars       :: [Variable],
    cfReadOnly        :: [Variable],
    cfDisallow        :: [Feature],
    cfTemplate        :: Template,
    cfConfFile        :: FilePath,
    cfThreads         :: Integer,
    cfEchoClingo      :: Bool,
    cfEchoASP         :: Bool,
    cfInteractive     :: Bool }
  deriving Show

defaultConf = Conf{
    cfIntRange        = error "int_range undefined",
    cfTimeMax         = error "time_max undefined",
    cfLineLimitMax    = Nothing,
    cfConstants       = [],
    cfProgramVars     = [],
    cfLogicVars       = [],
    cfReadOnly        = [],
    cfDisallow        = [],
    cfTemplate        = [],
    cfConfFile        = undefined,
    cfThreads         = 1,
    cfEchoClingo      = False,
    cfEchoASP         = False,
    cfInteractive     = False }

readConfFile :: FilePath -> Conf -> IO Conf
readConfFile filePath conf = do
    let options = runClingoOptions{ rcEchoStdout=False }
    result <- runClingo options [CIFile filePath]
    case result of
        CRSatisfiable (answer:_) -> do
            let conf' = readConfFacts answer conf
            return $ conf'{ cfConfFile=filePath }
        CRUnsatisfiable ->
            error $ filePath ++ " unsatisfiable"

readConfFacts :: [Fact] -> Conf -> Conf
readConfFacts facts conf
  = (readConfFacts' conf facts){ cfTemplate=template }
  where
    template = map snd . sortBy (compare `on` fst) $ do
        Fact (Name "template") [TInt n, tline] <- facts
        return (n, readTemplateLine tline)
    readConfFacts' conf (fact : facts) = case fact of
        Fact (Name "int_range") [TInt imin, TInt imax] ->
            readConfFacts' (conf{ cfIntRange=(imin, imax) }) facts
        Fact (Name "time_limit") [TInt tmax] ->
            readConfFacts' (conf{ cfTimeMax=tmax }) facts
        Fact (Name "line_limit_max") [TInt lmax] ->
            readConfFacts' (conf{ cfLineLimitMax=Just lmax }) facts
        Fact (Name "constant") [TInt con] ->
            readConfFacts' (conf{ cfConstants=con : cfConstants conf }) facts
        Fact (Name "program_variable") [TFun (Name var) []] ->
            readConfFacts' (conf{ cfProgramVars=var : cfProgramVars conf }) facts
        Fact (Name "logic_variable") [TFun (Name var) []] ->
            readConfFacts' (conf{ cfLogicVars=var : cfLogicVars conf }) facts
        Fact (Name "read_only_variable") [TFun (Name var) []] ->
            readConfFacts' (conf{ cfReadOnly=var : cfReadOnly conf }) facts
        Fact (Name "disallow_feature") [TFun (Name feat) []] ->
            readConfFacts' (conf{ cfDisallow=feat : cfDisallow conf }) facts
        Fact (Name "echo_clingo") [] ->
            readConfFacts' (conf{ cfEchoClingo=True }) facts
        Fact (Name "echo_asp") [] ->
            readConfFacts' (conf{ cfEchoASP=True }) facts
        Fact (Name "thread_count") [TInt tcount] ->
            readConfFacts' (conf{ cfThreads=tcount }) facts
        _ ->
            readConfFacts' conf facts
    readConfFacts' conf [] = conf

readTemplateLine :: Term -> TemplateLine
readTemplateLine line = case line of
    (TFun (Name "pre")   [TStr cond]) -> TLPre cond
    (TFun (Name "mid")   [TStr cond]) -> TLMid cond
    (TFun (Name "post")  [TStr cond]) -> TLPost cond
    (TFun (Name "instr") [instr])     -> TLInstr instr
    _ -> error $ "invalid template line: " ++ show line

--------------------------------------------------------------------------------
main = do
    args <- getArgs
    let ([confPath], conf) = readArgs args defaultConf
    conf@Conf{cfTemplate=tmpl} <- readConfFile confPath conf
    void $ templateLearn conf tmpl

readArgs :: [String] -> Conf -> ([String], Conf)
readArgs args@(arg : _) conf | "-" `isPrefixOf` arg
  = readArgsFlag  args conf
readArgs (arg : args) conf | otherwise
  = let (args', conf') = readArgs args conf in (arg:args', conf')
readArgs [] conf
  = ([], conf)

readArgsFlag (arg : args) conf
  | arg `elem` ["-i", "--interactive"]
  = readArgs args (conf{ cfInteractive=True })
  | arg `elem` ["--echo-clingo", "-ec"]
  = readArgs args (conf{ cfEchoClingo=True })
  | arg `elem` ["--echo-asp", "-ea"]
  = readArgs args (conf{ cfEchoASP=True })
  | arg `elem` ["-j", "--threads"]
  = let param : args' = args in
    readArgs args' (conf{ cfThreads = read param })
  | "-j" `isPrefixOf` arg
  = let Just param = stripPrefix "-j" arg in
    readArgs args (conf{ cfThreads = read param })
  | "--threads=" `isPrefixOf` arg
  = let Just param = stripPrefix "--threads" arg in
    readArgs args (conf{ cfThreads = read param })
  | otherwise
  = error $ "Unrecognised option: " ++ arg

--------------------------------------------------------------------------------
-- As templateLearn, but derives the template from the given configuration.
templateLearnConf :: Conf -> IO Program
templateLearnConf conf
  = templateLearn conf (cfTemplate conf)

--------------------------------------------------------------------------------
-- With respect to the given configuration, returns a program if one exists
-- satisfying the given template; otherwise, exits with failure.
templateLearn :: Conf -> Template -> IO Program
templateLearn conf tmpl = do
    prog <- Program . program 1 <$> templateLearn' conf (normaliseTemplate tmpl)
    putStrLn "The following program satisfies all conditions of the template:"
    printProgram prog
    return prog
  where
    -- The program starting at the given line number, with the given
    -- instructions, which may contain while loops with body length 'auto'.
    program :: While.LineNumber -> [Instruction] -> [LineInstr]
    program lNum (TFun (Name "while") [guard, TFun (Name "auto") []] : instrs)
      = Fact (Name "line_instr") [TInt lNum, instr] : program (lNum + 1) instrs
      where
        instr = TFun (Name "while") [guard, TInt (genericLength body)]
        (body, tail) = whileBody instrs
    program lNum (instr : instrs)
      = Fact (Name "line_instr") [TInt lNum, instr] : program (lNum + 1) instrs
    program _ []
      = []

    -- Given a list of instructions prefixed with a while loop body (i.e.
    -- starting after the header), gives the instructions in the body,
    -- as well as the remaining instructions starting at 'end_while'
    whileBody :: [Instruction] -> ([Instruction], [Instruction])
    whileBody (instr@(TFun (Name "while") _) : instrs)
      = let (body, tail)   = whileBody instrs in
        let (body', tail') = whileBody tail in
        (instr : body ++ body', tail')
    whileBody (instr@(TFun (Name "end_while") _) : instrs)
      = ([], instr : instrs)
    whileBody (instr : instrs)
      = let (body, tail) = whileBody instrs in
        (instr : body, tail)
    whileBody []
      = error "whileBody: no end_while instruction found"

--------------------------------------------------------------------------------
-- As templateLearn, but returns a list of instructions possibly containing
-- 'auto' as the body-length parameter of while loops, and requires the template
-- to be normalised, as by normaliseTemplate.
templateLearn' :: Conf -> Template -> IO [Instruction]
templateLearn' conf tmpl = case tmpl of
  TLMid _ : tmpl'@(TLInstr _ : _) -> do
    templateLearn' conf tmpl'
  TLInstr instr : tmpl' -> do
    instrs <- templateLearn' conf tmpl'
    return (instr : instrs)
  TLMid preCond : tmpl'@(TLMid postCond : _) -> do
    instrs <- templateLearn'' conf (preCond, postCond)
    instrs' <- templateLearn' conf tmpl'
    return (instrs ++ instrs')
  [TLMid _] -> do
    return []

--------------------------------------------------------------------------------
-- As templateLearn', but returns only the program fragment between a given
-- (precondition, postcondition) pair.
templateLearn'' :: Conf -> (Condition, Condition) -> IO [Instruction]
templateLearn'' conf (preCond, postCond) = do
    putStrLn $ "Synthesising the program fragment between conditions:"
    putStrLn $ "   Pre:  " ++ preCond
    putStrLn $ "   Post: " ++ postCond
    
    let iterativeConf = IL.defaultConf{
        IL.cfIntRange      = cfIntRange conf,
        IL.cfTimeMax       = cfTimeMax conf,
        IL.cfLineLimitMax  = cfLineLimitMax conf,
        IL.cfConstants     = cfConstants conf,
        IL.cfDisallow      = cfDisallow conf,
        IL.cfConfFile      = cfConfFile conf,
        IL.cfThreads       = cfThreads conf,
        IL.cfEchoClingo    = cfEchoClingo conf,
        IL.cfEchoASP       = cfEchoASP conf,
        IL.cfInteractive   = cfInteractive conf,
        IL.cfLogicVars     = cfLogicVars conf,
        IL.cfReadOnly      = cfReadOnly conf,
        IL.cfInputVars     = inputVars,
        IL.cfOutputVars    = outputVars,
        IL.cfExtraVars     = extraVars,
        IL.cfPreCondition  = iterativePreCond,
        IL.cfPostCondition = iterativePostCond }
    Program lineInstrs <- iterativeLearnConf iterativeConf

    putStrLn ""
    let lineInstrs' = [(l,i) | Fact (Name "line_instr") [TInt l, i] <- lineInstrs]
    return $ map snd . sortBy (compare `on` fst) $ lineInstrs'
  where
    preVars  = map (headMap toLower) (freeVariables preCond)
    postVars = map (headMap toLower) (freeVariables postCond)
    inputVars = nub . sort $
        filter (`elem` (preVars ++ postVars)) (cfLogicVars conf) ++
        filter (`elem` postVars)  (cfProgramVars conf)
    outputVars =
        filter (`elem` postVars) (cfProgramVars conf) \\ cfReadOnly conf
    extraVars = cfProgramVars conf \\ (inputVars ++ outputVars)
    iterativePreCond  = mapFreeVariables (("In_" ++) . headMap toLower) preCond
    iterativePostCond = mapFreeVariables mapOut postCond
    mapOut v
      | v' `elem` cfLogicVars conf                                = "In_" ++ v'
      | v' `elem` cfReadOnly conf && v' `elem` cfProgramVars conf = "In_" ++ v'
      | v' `elem` cfProgramVars conf                              = "Out_" ++ v'
      | otherwise                                                 = v
      where v' = headMap toLower v

--------------------------------------------------------------------------------
-- Normalises a program template by:
-- 1. Rejecting templates containing misplaced preconditions/postconditions.
-- 2. Inserting an empty precondition and/or postcondition if there are none.
-- 3. Encoding the precondition/postcondition as midconditions.
normaliseTemplate :: Template -> Template
normaliseTemplate tmpl
  | or [True | TLPre _ <- drop 1 tmpl]
  = error $ "precondition not at beginning of template"
  | or [True | TLPost _ <- drop 1 $ reverse tmpl]
  = error $ "postcondition not at end of template"
  | null [c | TLPre c <- take 1 tmpl]
  = normaliseTemplate $ [TLPre ""] ++ tmpl
  | null [c | TLPost c <- take 1 $ reverse tmpl]
  = normaliseTemplate $ tmpl ++ [TLPost ""]
  | otherwise
  = map normaliseLine tmpl
  where
    normaliseLine (TLPre cond)  = TLMid cond
    normaliseLine (TLPost cond) = TLMid cond
    normaliseLine l             = l

--------------------------------------------------------------------------------
interactivePause :: Conf -> IO Conf
interactivePause conf
  = case cfInteractive conf of
      True -> do
        putStr "\27[1m>\27[0m "
        line <- getLine
        case map toLower line of
            ""  -> return conf
            "q" -> exitSuccess
            _   -> showInteractiveHelp >> interactivePause conf
      False -> do
        return conf

showInteractiveHelp :: IO ()
showInteractiveHelp = do
    putStrLn $ "Type enter to continue or q to quit."

--------------------------------------------------------------------------------
headMap :: (a -> a) -> [a] -> [a]
headMap f (x : xs) = f x : xs
headMap _ _        = []
