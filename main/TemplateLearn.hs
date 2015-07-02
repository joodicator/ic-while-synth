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
import System.IO
import System.Environment
import System.Exit

--------------------------------------------------------------------------------
type Template = Template [TemplateLine]

data TemplateLine
  = TLPre Condition
  | TLMid Condition
  | TLPost Condition
  | TLInstr Term
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
            error $ filePath ++ " unsatisfiable."

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
    templateLearn conf tmpl

templateLearn :: Conf -> Template -> IO Program
templateLearn conf tmpl
  = Program . program <$> templateLearn' conf (normaliseTemplate tmpl)
 where 
    program :: [Instr] -> [LineInstr]
    program (TFun (Name "while") [guard, TFun (Name "auto") []] : instrs)
      = let (body, tail) = whileBody instrs in
        TFun (Name "while") [guard, length body] : program instrs
    program (instr : instrs)
      = instr : program instrs

    whileBody :: [Instr] -> ([Instr], [Instr])
    whileBody (instr@(TFun (Name "while") _) : instrs)
      = let (body, tail)   = whileBody instrs in
        let (body', tail') = whileBody tail in
        (instr : body ++ body', tail')
    whileBody (instr@(TFun (Name "end_while") _) : instrs)
      = ([], instr : instrs)
    whileBody (instr : instrs)
      = let (body, tail) = whileBody instrs in
        (instr : body, tail)

templateLearn' :: Conf -> Template -> IO [Instr]
templateLearn' conf tmpl = case tmpl of
  TLMid _ : tmpl'@(TLInstr _ : _) -> do
    templateLearn' conf tmpl'
  TLInstr instr : tmpl' -> do
    instrs <- templateLearn' conf tmpl'
    return (instr : instrs)
  TLMid preCond : TLMid postCond : tmpl' -> do
    instrs <- templateLearn'' conf (preCond, postCond)
    instrs' <- templateLearn' conf tmpl'
    return (instrs ++ instrs')
  [] -> do
    return []

templateLearn'' :: Conf -> (Cond, Cond) -> IO [Instr]
templateLearn'' conf (preCond, postCond) = do
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
        IL.cfInterative    = cfInteractive conf}
        IL.cfLogicVars     = cfLogicVars conf,
        IL.cfReadOnly      = cfReadOnly conf,
        IL.cfInputVars     = inputVars,
        IL.cfOutputVars    = outputVars,
        IL.cfExtraVars     = extraVars,
        IL.cfPreCondition  = iterativePreCond,
        IL.cfPostCondition = iterativePostCond,
    Program lineInstrs <- iterativeLearnConf conf
    return [i | Fact (Name "line_instr") [_, i] <- lineInstrs]
  where
    inputVars    = nub . sort $ freeVariables preCond ++ freeVariables postCond
    outputVars   = freeVariables postCond \\ cfLogicVars conf
    extraVars    = cfProgramVars conf \\ (inputVars ++ outputVars)

normaliseTemplate :: Template -> Template
normaliseTemplate tmpl
  | and [True | TLPre _ <- drop 1 tmpl]
  = error $ "precondition not at beginning of template"
  | and [True | TLPost _ <- drop 1 $ reverse tmpl]
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
