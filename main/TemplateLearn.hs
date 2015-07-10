#!/usr/bin/env runghc

{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

import Clingo hiding (readArgs)
import qualified While

import IterativeLearn hiding (
    main, Conf(..), defaultConf, readConfFacts, readConfFile,
    readArgs, readArgsFlag, interactivePause, showInteractiveHelp)
import qualified IterativeLearn as IL

import Template
import Logic

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
data Conf = Conf{
    cfIntRange        :: (Integer, Integer),
    cfTimeMax         :: Integer, 
    cfLineLimitMax    :: Maybe Integer,
    cfConstants       :: [Value],   
    cfReadOnly        :: [Variable],
    cfDisallow        :: [Feature],
    cfIfStatementsMax :: Maybe Integer,
    cfWhileLoopsMax   :: Maybe Integer,
    cfProgramVars     :: [Variable],
    cfLogicVars       :: [Variable],
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
    cfReadOnly        = [],
    cfDisallow        = [],
    cfIfStatementsMax = Nothing,
    cfWhileLoopsMax   = Nothing,
    cfProgramVars     = [],
    cfLogicVars       = [],
    cfTemplate        = emptyTemplate,
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
  = (readConfFacts' conf facts){
        cfTemplate    = template,
        cfProgramVars = tcProgramVars . teConf $ template,
        cfLogicVars   = tcLogicVars   . teConf $ template,
        cfReadOnly    = tcReadOnly    . teConf $ template }
  where
    template  = either error id $ answerToTemplate facts

    readConfFacts' conf (fact : facts) = case fact of
        Fact (Name "int_range") [TInt imin, TInt imax] ->
            readConfFacts' (conf{ cfIntRange=(imin, imax) }) facts
        Fact (Name "time_limit") [TInt tmax] ->
            readConfFacts' (conf{ cfTimeMax=tmax }) facts
        Fact (Name "line_limit_max") [TInt lmax] ->
            readConfFacts' (conf{ cfLineLimitMax=Just lmax }) facts
        Fact (Name "constant") [TInt con] ->
            readConfFacts' (conf{ cfConstants=con : cfConstants conf }) facts
        Fact (Name "disallow_feature") [TFun (Name feat) []] ->
            readConfFacts' (conf{ cfDisallow=feat : cfDisallow conf }) facts
        Fact (Name "if_statements_max") [TInt fmax] ->
            readConfFacts' (conf{ cfIfStatementsMax=Just fmax }) facts
        Fact (Name "while_loops_max") [TInt wmax] ->
            readConfFacts' (conf{ cfWhileLoopsMax=Just wmax }) facts
        Fact (Name "echo_clingo") [] ->
            readConfFacts' (conf{ cfEchoClingo=True }) facts
        Fact (Name "echo_asp") [] ->
            readConfFacts' (conf{ cfEchoASP=True }) facts
        Fact (Name "thread_count") [TInt tcount] ->
            readConfFacts' (conf{ cfThreads=tcount }) facts
        _ ->
            readConfFacts' conf facts
    readConfFacts' conf [] = conf

--------------------------------------------------------------------------------
main = do
    args <- getArgs
    let ([confPath], conf) = readArgs args defaultConf
    conf <- readConfFile confPath conf
    void $ templateLearnConf conf

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
templateLearn conf tmpl@Template{ teParts=parts } = do
    putStrLn "The following template was specified:"
    printTemplate tmpl
    putStrLn ""

    let tmpl' = normaliseTemplate $ tmpl
    putStrLn "Which was expanded into the following 'normal' form:"
    printTemplate tmpl'
    putStrLn ""

    let tmpl'' = completeTemplate $ tmpl'
    putStrLn "Which was further expanded into the following 'complete' form:"
    printTemplate tmpl''
    putStrLn ""

    let conf' = conf{
        cfTemplate    = tmpl'',
        cfProgramVars = tcProgramVars . teConf $ tmpl'',
        cfLogicVars   = tcLogicVars   . teConf $ tmpl'',
        cfReadOnly    = tcReadOnly    . teConf $ tmpl'' }

    prog <- Program . program 1 <$> templateLearn' conf' (teParts tmpl'')
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
-- 'auto' as the body-length parameter of while loops.
templateLearn' :: Conf -> [TePart] -> IO [Instruction]
templateLearn' conf parts = case parts of
  TPInstr (TISet var expr) : tmpl' -> do
    let exprTerm = While.exprToTerm expr
    (TFun "set" [TFun var [], exprTerm] :) <$> templateLearn' conf tmpl'
  TPBlock BTIf guard body : tmpl' -> do
    -- Subprogram inside an if body.
    frag <- templateLearn' conf body
    let guardTerm = While.guardToTerm guard
    let head = TFun "if" [guardTerm, TInt (genericLength frag)]
    ((head : frag) ++) <$> templateLearn' conf tmpl'
  TPBlock BTWhile guard body : tmpl' -> do
    -- Subprogram inside a while body.
    frag <- templateLearn' conf body
    let guardTerm = While.guardToTerm guard
    let head = TFun "while" [guardTerm, TInt (genericLength frag)]
    ((head : frag ++ [TFun "end_while" []]) ++) <$> templateLearn' conf tmpl'
  TPCond _ preCond : tmpl'@(TPCond _ postCond : _) -> do
    -- Subprogram between two conditions.
    frag <- templateLearn'' conf (preCond, postCond)
    (frag ++) <$> templateLearn' conf tmpl'
  _ : tmpl' ->
    -- Anything else ignored.  
    templateLearn' conf tmpl'
  [] ->
    return []

--------------------------------------------------------------------------------
-- As templateLearn', but returns only the program fragment between a given
-- (precondition, postcondition) pair.
templateLearn'' :: Conf -> (Condition, Condition) -> IO [Instruction]
templateLearn'' conf (preCond, postCond) = do
    let preCondStr  = if null preCond  then "(none)" else preCond
    let postCondStr = if null postCond then "(none)" else postCond
    putStrLn $ "Synthesising the program fragment between conditions:"
    putStrLn $ "   Pre:  " ++ preCondStr  ++ "."
    putStrLn $ "   Post: " ++ postCondStr ++ "."
    
    let iterativeConf = IL.defaultConf{
        IL.cfIntRange        = cfIntRange conf,
        IL.cfTimeMax         = cfTimeMax conf,
        IL.cfLineLimitMax    = cfLineLimitMax conf,
        IL.cfConstants       = cfConstants conf,
        IL.cfDisallow        = cfDisallow conf,
        IL.cfIfStatementsMax = cfIfStatementsMax conf,
        IL.cfWhileLoopsMax   = cfWhileLoopsMax conf,
        IL.cfConfFile        = cfConfFile conf,
        IL.cfThreads         = cfThreads conf,
        IL.cfEchoClingo      = cfEchoClingo conf,
        IL.cfEchoASP         = cfEchoASP conf,
        IL.cfInteractive     = cfInteractive conf,
        IL.cfLogicVars       = cfLogicVars conf,
        IL.cfReadOnly        = cfReadOnly conf,
        IL.cfInputVars       = inputVars,
        IL.cfOutputVars      = outputVars,
        IL.cfExtraVars       = extraVars,
        IL.cfPreCondition    = iterativePreCond,
        IL.cfPostCondition   = iterativePostCond }
    Program lineInstrs <- iterativeLearnConf iterativeConf

    putStrLn ""
    let lineInstrs' = [(l,i) | Fact (Name "line_instr") [TInt l, i] <- lineInstrs]
    return $ map snd . sortBy (compare `on` fst) $ lineInstrs'
  where
    preVars  = map (headMap toLower) (freeVariables preCond)
    postVars = map (headMap toLower) (freeVariables postCond)
    inputVars = nub . sort $
        filter (`elem` preVars) (cfLogicVars conf) ++
        filter (`elem` (preVars ++ postVars)) (cfProgramVars conf)
    outputVars =
        filter (`elem` postVars) (cfProgramVars conf) \\ cfReadOnly conf
    extraVars = cfProgramVars conf \\ (inputVars ++ outputVars)
    iterativePreCond  = mapFreeVariables mapIn preCond
    iterativePostCond = mapFreeVariables mapOut postCond
    mapIn v
      | v' `elem` inputVars  = "In_" ++ v'
      | otherwise            = v
      where v' = headMap toLower v
    mapOut v
      | v' `elem` outputVars = "Out_" ++ v'
      | v' `elem` inputVars  = "In_" ++ v'
      | otherwise            = v
      where v' = headMap toLower v

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
