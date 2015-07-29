#!/usr/bin/env runghc

{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module IterativeLearn where

import Clingo hiding (readArgs)
import qualified While
import Logic

import Control.Applicative
import Control.Monad
import Control.Concurrent
import qualified Control.Concurrent.MSem as MSem

import Data.List
import Data.Maybe
import Data.Char

import System.IO
import System.Environment
import System.Exit

type Value   = Integer
type Feature = String

type ArrayName = String
type ArraySize = Integer

type LineInstr   = Clingo.Fact
type Instruction = Clingo.Term

newtype Program = Program [LineInstr] deriving Show
newtype Input   = Input   [(WhileVar, Value)] deriving (Show, Eq, Ord)
newtype Output  = Output  [(WhileVar, Value)] deriving (Show, Eq, Ord)

data WhileVar
  = VScalar Variable
  | VArray ArrayName Integer
  deriving (Eq, Ord)

data Limits = Limits{
    lmLineMax :: Integer }

data Example
  = UserExample{
        exID     :: String,
        exArrays :: [(ArrayName, ArraySize)],
        exInput  :: Input,
        exOutput :: Output }
  | PostConditionExample {
        exID     :: String,
        exArrays :: [(ArrayName, ArraySize)],
        exInput  :: Input }
  deriving Show

data Counterexample = Counterexample {
    ceArrays         :: [(ArrayName, ArraySize)],
    ceInput          :: Input,
    ceActualOutput   :: Output,
    ceExpectedOutput :: [Output] }
  deriving Show

--------------------------------------------------------------------------------
data Conf = Conf{
    cfIntRange        :: (Integer, Integer),
    cfTimeMax         :: Integer, 
    cfLineLimitMin    :: Integer,
    cfLineLimitStep   :: Integer,
    cfLineLimitMax    :: Maybe Integer,
    cfIfStatementsMax :: Maybe Integer,
    cfWhileLoopsMax   :: Maybe Integer,
    cfConstants       :: [Value],   
    cfDisallow        :: [Feature],

    cfInputVars       :: [WhileVar],
    cfOutputVars      :: [WhileVar],
    cfExtraVars       :: [WhileVar],
    cfLogicVars       :: [WhileVar],
    cfReadOnly        :: [WhileVar],

    cfArrays          :: [(ArrayName, ArraySize)],
    cfInputArrays     :: [ArrayName],
    cfOutputArrays    :: [ArrayName],
    cfReadOnlyArrays  :: [ArrayName],

    cfExamples        :: [Example],
    cfPreCondition    :: Condition,
    cfPostCondition   :: Condition,

    cfConfFile        :: FilePath,
    cfThreads         :: Integer,
    cfEchoClingo      :: Bool,
    cfEchoASP         :: Bool,
    cfInteractive     :: Bool,
    cfPutLines        :: [String] -> IO () }

defaultConf :: Conf
defaultConf = Conf{
    cfIntRange        = error "int_range undefined",
    cfTimeMax         = error "time_max undefined",
    cfLineLimitMin    = 0,
    cfLineLimitStep   = 1,
    cfLineLimitMax    = Nothing,
    cfIfStatementsMax = Nothing,
    cfWhileLoopsMax   = Nothing,
    cfConstants       = [],
    cfDisallow        = [],

    cfInputVars       = [],
    cfOutputVars      = [],
    cfExtraVars       = [],
    cfLogicVars       = [],
    cfReadOnly        = [],

    cfArrays          = [],
    cfInputArrays     = [],
    cfOutputArrays    = [],
    cfReadOnlyArrays  = [],

    cfExamples        = [],
    cfPreCondition    = "",
    cfPostCondition   = "",

    cfConfFile        = undefined,
    cfThreads         = 1,
    cfEchoClingo      = False,
    cfEchoASP         = False,
    cfInteractive     = False,
    cfPutLines        = mapM_ putStrLn }

readConfFile :: FilePath -> Conf -> IO Conf
readConfFile filePath conf = do
    let options = runClingoOptions{ rcEchoStdout=False }
    let code = "#const line_max=0."
    result <- runClingo options [CICode code, CIFile filePath]
    case result of
        CRSatisfiable (answer:_) -> do
            let conf' = readConfFacts answer conf
            return $ conf'{ cfConfFile=filePath }
        CRUnsatisfiable ->
            error $ filePath ++ " unsatisfiable."
        _ -> error "impossible"

readConfFacts :: [Fact] -> Conf -> Conf
readConfFacts inFacts inConf
  = (readConfFacts' inConf inFacts){ cfExamples=examples }
  where
    xArrays = [(e, a, l)
        | Fact "array" [TFun(Name e)[], TFun(Name a)[], TInt l] <- inFacts]
    ins = [(e, v, c)
        | Fact (Name "in") [TFun(Name e)[], vt, TInt c] <- inFacts,
          v <- maybeToList $ readVarTerm vt]
    outs = [(e, v, c)
        | Fact (Name "out") [TFun(Name e)[], vt, TInt c] <- inFacts,
          v <- maybeToList $ readVarTerm vt]
    examples = [UserExample{
        exID     = e,
        exArrays = [(a,l) | (e',a,l) <- xArrays, e == e'],
        exInput  = Input  [(v,c) | (e',v,c) <- ins,  e == e'],
        exOutput = Output [(v,c) | (e',v,c) <- outs, e == e'] }
        | e <- nub . sort $ [e | (e,_,_) <- ins ++ outs]
                          ++[e | (e,_,_) <- xArrays]]
    readConfFacts' conf (fact : facts) = case fact of
        Fact (Name "int_range") [TInt imin, TInt imax] ->
            readConfFacts' (conf{ cfIntRange=(imin, imax) }) facts
        Fact (Name "time_limit") [TInt tmax] ->
            readConfFacts' (conf{ cfTimeMax=tmax }) facts
        Fact (Name "line_limit_min") [TInt lmin] ->
            readConfFacts' (conf{ cfLineLimitMin=lmin }) facts
        Fact (Name "line_limit_step") [TInt lstep] ->
            readConfFacts' (conf{ cfLineLimitStep=lstep }) facts
        Fact (Name "line_limit_max") [TInt lmax] ->
            readConfFacts' (conf{ cfLineLimitMax=Just lmax }) facts
        Fact (Name "if_statements_max") [TInt fmax] ->
            readConfFacts' (conf{ cfIfStatementsMax=Just fmax }) facts
        Fact (Name "while_loops_max") [TInt wmax] ->
            readConfFacts' (conf{ cfWhileLoopsMax=Just wmax }) facts
        Fact (Name "constant") [TInt con] ->
            readConfFacts' (conf{ cfConstants=con : cfConstants conf }) facts
        Fact (Name "input_variable") [readVarTerm -> Just var] ->
            readConfFacts' (conf{ cfInputVars=var : cfInputVars conf }) facts
        Fact (Name "output_variable") [readVarTerm -> Just var] ->
            readConfFacts' (conf{ cfOutputVars=var : cfOutputVars conf }) facts
        Fact (Name "extra_variable") [readVarTerm -> Just var] ->
            readConfFacts' (conf{ cfExtraVars=var : cfExtraVars conf }) facts
        Fact (Name "read_only_variable") [readVarTerm -> Just var] ->
            readConfFacts' (conf{ cfReadOnly=var : cfReadOnly conf }) facts
        Fact (Name "disallow_feature") [TFun (Name feat) []] ->
            readConfFacts' (conf{ cfDisallow=feat : cfDisallow conf }) facts
        Fact (Name "precondition") [TStr cond] ->
            readConfFacts' (conf{ cfPreCondition=cond }) facts
        Fact (Name "postcondition") [TStr cond] ->
            readConfFacts' (conf{ cfPostCondition=cond }) facts
        Fact (Name "echo_clingo") [] ->
            readConfFacts' (conf{ cfEchoClingo=True }) facts
        Fact (Name "echo_asp") [] ->
            readConfFacts' (conf{ cfEchoASP=True }) facts
        Fact (Name "thread_count") [TInt tcount] ->
            readConfFacts' (conf{ cfThreads=tcount }) facts
        Fact (Name "array") [TFun(Name name)[], TInt size] ->
            readConfFacts' (conf{
                cfArrays = (name,size) : cfArrays conf }) facts
        Fact (Name "input_array") [TFun(Name name)[]] ->
            readConfFacts' (conf{
                cfInputArrays = name : cfInputArrays conf }) facts
        Fact (Name "output_array") [TFun(Name name)[]] ->
            readConfFacts' (conf{
                cfOutputArrays = name : cfOutputArrays conf }) facts
        Fact (Name "read_only_array") [TFun(Name name)[]] ->
            readConfFacts' (conf{
                cfReadOnlyArrays = name : cfReadOnlyArrays conf }) facts
        _ ->
            readConfFacts' conf facts
    readConfFacts' conf [] = conf

--------------------------------------------------------------------------------
main :: IO ()
main = do
    args <- getArgs
    let ([confPath], conf) = readArgs args defaultConf
    _conf <- readConfFile confPath conf
    void $ iterativeLearnConf _conf

readArgs :: [String] -> Conf -> ([String], Conf)
readArgs args@(arg : _) conf | "-" `isPrefixOf` arg
  = readArgsFlag  args conf
readArgs (arg : args) conf | otherwise
  = let (args', conf') = readArgs args conf in (arg:args', conf')
readArgs [] conf
  = ([], conf)

readArgsFlag :: [String] -> Conf -> ([String], Conf)
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
  = let Just param = stripPrefix "--threads=" arg in
    readArgs args (conf{ cfThreads = read param })
  | otherwise
  = error $ "Unrecognised option: " ++ arg
readArgsFlag [] conf
  = ([], conf)

--------------------------------------------------------------------------------
iterativeLearnConf :: Conf -> IO Program
iterativeLearnConf conf = do
    let examples = cfExamples conf
    let limits = Limits{ lmLineMax = cfLineLimitMin conf }
    iterativeLearn examples limits conf

iterativeLearn :: [Example] -> Limits -> Conf -> IO Program
iterativeLearn exs lims conf = do
    let limss = [lims{ lmLineMax=l } | i <- [0..cfThreads conf-1],
                 let l = lmLineMax lims + i * cfLineLimitStep conf,
                 maybe True (l <=) (cfLineLimitMax conf)]

    let linesString = case limss of {
        [_] -> show(lmLineMax $ head limss);
        _   -> show(lmLineMax $ head limss)++"-"++show(lmLineMax $ last limss) }
    cfPutLines conf ["Searching for a program with " ++ linesString ++ " lines"
                 ++ " satisfying " ++ show (length exs) ++ " example(s)..."]

    mProg <- findProgramConcurrent exs limss conf

    case mProg of
      Just prog -> do
        cfPutLines conf $ ["Found the following program:"] ++ showProgram prog
        _conf <- interactivePause conf
        let lineMax = programLength prog
        case cfPostCondition _conf of
            _:_ -> iterativeLearn' prog exs lims{lmLineMax = lineMax} _conf
            ""  -> return prog
      Nothing -> do
        let lims' = last limss
        let lineMax = lmLineMax lims' + cfLineLimitStep conf
        case cfLineLimitMax conf of
            Just limit | lineMax > limit -> do
                cfPutLines conf ["Failure: no such program can be found within the"
                               ++ " configured line limits."]
                exitFailure
            _ -> do
                cfPutLines conf ["No such program found."]
                iterativeLearn exs (lims'{lmLineMax=lineMax}) conf

iterativeLearn' :: Program -> [Example] -> Limits -> Conf -> IO Program
iterativeLearn' prog exs lims conf = do
    cfPutLines conf [
        "Searching for a counterexample to falsify the postcondition..."]
    mCounter <- findCounterexample prog conf
    case mCounter of
        Just cex -> do
            let Counterexample{
                ceArrays         = arrays,
                ceInput          = Input input,
                ceActualOutput   = Output actual,
                ceExpectedOutput = expected } = cex

            let deficient = null expected
            headLines <- return $ case deficient of
                True  -> [
                    "Failure: counterexamples were found, but none admitting a"
                    ++ " solution the postcondition. Possible causes:",
                    "- The postcondition is unsatisfiable for some valid input.",
                    "- The precondition is satisfiable for some invalid input.",
                    "- The given int_range is too small."]
                False -> [
                    "Found the following counterexample:"]

            bodyLines <- return $ ("   " ++) <$>
                (guard (not . null $ arrays) >> [
                "Arrays:   " ++ intercalate ", " [
                    a ++"["++ show l ++"]" | (a,l) <- arrays]]) ++ [
                "Input:    " ++ (intercalate ", "
                    $ [show v++"="++show c | (v,c) <- sort input ]),
                "Expected: " ++ case expected of {
                    [] -> "(none)";
                    _  -> intercalate " | " $ [
                        intercalate ", " [show v ++ "=" ++ show c | (v,c) <- os]
                        | Output os <- take 5 $ sort [
                            Output (sort os) | Output os <- expected]]
                        ++ if (length expected > 5) then ["..."] else [] },
                "Output:   " ++ case actual of {
                    [] -> "(none)";
                    _  -> intercalate ", "
                        $ [show v++"="++show c | (v,c) <- sort actual ] } ]

            cfPutLines conf $ headLines ++ bodyLines
            when deficient exitFailure
            _conf <- interactivePause conf

            let ex = PostConditionExample{
                exArrays = arrays,
                exID     = head $ map (("cx"++) . show) [1::Int ..] \\ map exID exs,
                exInput  = Input input }
            iterativeLearn (ex : exs) lims _conf
        Nothing -> do
            cfPutLines conf [
                "Success: the postcondition could not be falsified."]
            return prog

--------------------------------------------------------------------------------
findProgram :: [Example] -> Limits -> Conf -> IO (Maybe Program)
findProgram exs lims conf = do
    let code = findProgramASP exs lims conf
    let maybeRunID = case cfThreads conf of {
        1 -> Nothing;
        _ -> Just (show $ lmLineMax lims) }
    result <- runClingoConf [CICode code] maybeRunID conf
    case result of
        CRSatisfiable (answer:_) ->
            return $ Just (Program answer)
        _ -> return Nothing

findProgramConcurrent :: [Example] -> [Limits] -> Conf -> IO (Maybe Program)
findProgramConcurrent exs limss conf = do
    outSem <- MSem.new (1 :: Integer)
    let conf' = conf{ cfPutLines = MSem.with outSem . cfPutLines conf }
    tasks <- forM limss $ \lims -> do
        result <- newEmptyMVar
        thread <- forkIO $ putMVar result =<< findProgram exs lims conf'
        return (thread, result)
    results <- forM (zip tasks [0..]) $ \((thread, resultMVar), i) -> do
        prior <- forM (take i tasks) $ \(_,r) -> do
            isEmpty <- isEmptyMVar r
            if isEmpty then return Nothing else readMVar r
        case any isJust prior of
          True  -> killThread thread >> return Nothing
          False -> readMVar resultMVar
    return (listToMaybe . catMaybes $ results)

findProgramASP :: [Example] -> Limits -> Conf -> String
findProgramASP exs lims conf
  = unlines $ concat [headerLines, biasLines, exampleLines, postCondLines]
  where
    headerLines = [
        "#const line_max=" ++ show lineMax ++ ".",
        "#const time_max=" ++ show timeMax ++ ".",
        "#const int_min=" ++ show intMin ++ ".",
        "#const int_max=" ++ show intMax ++ ".",
        "#const if_max=" ++ maybe "any" show ifMax ++ ".",
        "#const while_max=" ++ maybe "any" show whileMax ++ ".",
        "#include \"learn.lp\".",
        "#hide. #show line_instr/2."]

    biasLines =
        (guard (not $ null constants) >>
            ["con(" ++ intercalate "; "
                (map show constants) ++ ")."]) ++
        (guard (not . null $ allVars \\ logicVars) >>
            ["var(" ++ intercalate "; "
                (varTerm <$> allVars \\ logicVars) ++ ")."]) ++
        (guard (not . null $ allVars \\ readOnly) >>
            ["write_var(" ++ intercalate "; "
                (varTerm <$> allVars \\ readOnly) ++  ")."]) ++
        (guard (not . null $ allArrays \\ roArrays) >>
            ["write_array("++ intercalate ";"
                (allArrays \\ roArrays) ++")."]) ++
        (guard (not $ null disallow) >>
            ["disallow(" ++ intercalate "; " disallow ++ ")."])

    exampleLines = do
        example <- exs
        let xID = exID example
        arrs <- return $ do
            (name, size) <- exArrays example
            return $ "array("++ xID ++","++ name ++","++ show size ++")."
        ins <- return $ do
            (var, val) <- let Input is = exInput example in is
            return $ "in(" ++ xID ++ "," ++ varTerm var ++ "," ++ show val ++ ")."
        outs <- return $ do
            UserExample{ exOutput=Output outps } <- return example
            (var, val) <- outps
            return $ "out(" ++ xID ++ "," ++ varTerm var ++ "," ++ show val ++ ")."
        return $ unwords $ arrs ++ ins ++ outs
       
    postCondLines =
        let ids = [xid
                  | PostConditionExample{ exID=xid } <- exs ] in
        let iVars = [v
                    | v <- inputVars, ("In_"++ varVar v) `isFreeIn` postCond] in
        let oVars = [v
                    | v <- outputVars, ("Out_"++ varVar v) `isFreeIn` postCond] in
        let plVars = [v
                     | v <- logicVars \\ outputVars, varVar v `isFreeIn` postCond] in
        if (null ids) then [] else [
        "postcon_run(" ++ intercalate ";" ids ++ ").",
        "postcon("++ intercalate "," ("R" : map (("Out_"++) . varVar) oVars) ++") :- "
        ++ "postcon_run(R), "
        ++ concat ["int(Out_"++ varVar v ++"), " | v <- oVars]
        ++ concat ["int("++ varVar v ++"), " | v <- plVars]
        ++ concat ["in(R,"++ varTerm v ++",In_"++ varVar v ++"), " | v <- iVars]
        ++ postCond ++".",
        ":- postcon_run(R), "
        ++ concat ["run_var_out(R,"++ varTerm v ++",Out_"++ varVar v ++"), " | v <- oVars]
        ++ "not postcon("++ intercalate "," ("R" : map (("Out_"++) . varVar) oVars) ++")."]

    allVars   = nub . sort $ inputVars ++ outputVars ++ extraVars
    allArrays = map fst arrays
    
    Conf{ cfTimeMax        =timeMax,   cfIntRange      =(intMin, intMax),
          cfInputVars      =inputVars, cfOutputVars    =outputVars,
          cfExtraVars      =extraVars, cfConstants     =constants,
          cfDisallow       =disallow,  cfReadOnly      =readOnly,
          cfIfStatementsMax=ifMax,     cfWhileLoopsMax =whileMax,
          cfLogicVars      =logicVars, cfPostCondition =postCond,
          cfArrays         =arrays,    cfReadOnlyArrays=roArrays } = conf
    Limits{ lmLineMax=lineMax } = lims
    
--------------------------------------------------------------------------------
findCounterexample :: Program -> Conf -> IO (Maybe Counterexample)
findCounterexample prog conf = do
    let code = findCounterexampleASP prog conf
    result <- runClingoConf [CICode code] Nothing conf
    case result of
        CRSatisfiable (answer : _) -> do
            eArrays <- return $ do
                Fact "counter_array" args <- answer
                let [TFun (Name aName) [], TInt aLen] = args
                guard (aName `elem` allArrays)
                return (aName, aLen)
            inputs <- return $ do
                Fact (Name "counter_in") args <- answer
                let [tName, TInt value] = args
                name <- maybeToList $ readVarTerm tName
                guard $ name `elem` inputVars
                return (name, value)
            outputs <- return $ do
                Fact (Name "counter_out") args <- answer
                [tName, TInt value] <- [args]
                name <- maybeToList $ readVarTerm tName
                guard $ name `elem` outputVars
                return (name, value)
            expected <- return $ do
                Fact (Name "postcon") args <- answer
                let names = [v | v <- outputVars,
                             ("Out_"++ varVar v) `isFreeIn` postCond]
                let values = [c | TInt c <- args]
                return $ Output (zip names values)
            return $ Just $ Counterexample{
                ceArrays         = eArrays,
                ceInput          = Input inputs,
                ceActualOutput   = Output outputs,
                ceExpectedOutput = expected }
        _ -> return Nothing
  where
    allArrays = [aName | (aName,_) <- arrays]
    Conf{ cfArrays=arrays, cfPostCondition=postCond,
          cfInputVars=inputVars, cfOutputVars=outputVars } = conf

findCounterexampleASP :: Program -> Conf -> String
findCounterexampleASP prog conf
  = unlines . intercalate [""] $ [
        headerLines, programLines, preCondLines, postCondLines]
  where
    headerLines = [
        "#const time_max=" ++ show timeMax ++ ".",
        "#const int_min=" ++ show intMin ++ ".",
        "#const int_max=" ++ show intMax ++ ".",
        "#const line_max=0.",
        "#include \"counterexample.lp\".",      
        "input_var("  ++ intercalate "; " (map varTerm inputVars)  ++ ").",
        "output_var(" ++ intercalate "; " (map varTerm outputVars) ++ ").",
        "input_array("  ++ intercalate ";" inputArrays ++ ").",
        "output_array(" ++ intercalate ";" outputArrays ++ ")."]

    programLines =
        let Program instrs = prog in Clingo.showFactLines instrs

    preCondLines =
        let preVars = filter ((`isFreeIn` preCond) . ("In_"++) . varVar) inputVars in
        let inDom   = ["counter_in("++ varTerm v ++", In_"++ varVar v ++")" | v<-preVars] in [
        let preConds = filter (not . null) [preCond] in
        "precon :- "++ intercalate ", " (preConds ++ inDom) ++".",
        ":- not precon."]
    
    postCondLines =
        let inVars = filter ((`isFreeIn` postCond) . ("In_"++) . varVar) inputVars in
        let inDom = ["counter_in("++ varTerm v ++", In_"++ varVar v ++")" | v<-inVars] in
        let plVars = [v | v <- (logicVars \\ outputVars),
                          varVar v `isFreeIn` postCond] in
        let expOutDom = ["int(Out_"++ varVar v ++")" | v <- outputVars]
                     ++ ["int("++ varTerm v ++")" | v <- plVars] in
        let postConds = filter (not . null) [postCond] in
        let ruleBody = intercalate ", " (postConds ++ inDom ++ expOutDom) in
        let outVars = filter ((`isFreeIn` postCond) . ("Out_"++) . varVar) outputVars in
        let args = case outVars of {
            [] -> "";
            _  -> "(" ++ intercalate ", " (map (("Out_"++) . varVar) outVars) ++ ")" } in
        let actOutDom = ["counter_out("++ varTerm v ++", Out_"++ varVar v ++")" | v<-outVars]
                     ++ ["int("++ varTerm v ++")" | v <- plVars] in
        let consHead = "postcon" ++ args in [
        "postcon"++ args ++" :- "++ ruleBody ++".",
        ":- " ++ intercalate ", " (consHead : actOutDom) ++ ".",
        "any_postcon :- postcon(" ++ intercalate "," ("_" <$ outVars) ++ ").",
        "#show postcon/" ++ show (length outVars) ++ "."]

    Conf{ cfIntRange=(intMin, intMax), cfTimeMax=timeMax,
          cfInputVars=inputVars, cfOutputVars=outputVars,
          cfInputArrays=inputArrays, cfOutputArrays=outputArrays,
          cfPreCondition=preCond, cfPostCondition=postCond,
          cfLogicVars=logicVars } = conf

--------------------------------------------------------------------------------
interactivePause :: Conf -> IO Conf
interactivePause conf
  = case cfInteractive conf of
      True -> do
        putStr "\27[1m>\27[0m "
        hFlush stdout
        line <- getLine
        case map toLower line of
            ""  -> return conf
            "c" -> return $ conf{ cfInteractive=False }
            "q" -> exitSuccess
            _   -> showInteractiveHelp >> interactivePause conf
      False -> do
        return conf

showInteractiveHelp :: IO ()
showInteractiveHelp
  = putStrLn "Type enter to continue, c to exit interactive mode, or q to quit."

--------------------------------------------------------------------------------
runClingoConf :: [ClingoInput] -> Maybe String -> Conf -> IO ClingoResult
runClingoConf input maybeRunID conf = do
    let options = runClingoOptions{
        rcEchoStdout = cfEchoClingo conf,
        rcEchoInput  = cfEchoASP conf,
        rcIdentifier = maybeRunID,
        rcEcho       = cfPutLines conf }
    runClingo options (input ++ [CIFile $ cfConfFile conf])

showProgram :: Program -> [String]
showProgram (Program [])
  = ["   (empty program)"]
showProgram (Program facts)
  = While.showProgram . catMaybes . map While.readLineInstr $ facts

programLength :: Program -> Integer
programLength (Program facts)
  = genericLength . catMaybes . map While.readLineInstr $ facts

readVarTerm :: Term -> Maybe WhileVar
readVarTerm term = case term of
    TFun (Name v) []                             -> Just $ VScalar v
    TFun (Name "array") [TFun(Name a)[], TInt i] -> Just $ VArray  a i
    _                                            -> Nothing

varTerm :: WhileVar -> String
varTerm (VScalar v)  = v
varTerm (VArray a i) = "array("++ a ++","++ show i ++")"

varVar :: WhileVar -> String
varVar (VScalar v)  = headUp v
varVar (VArray a i) = headUp a ++ show i

instance Show WhileVar where
    show (VScalar v)  = v
    show (VArray a i) = a ++"["++ show i ++"]"
