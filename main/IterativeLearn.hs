#!/usr/bin/env runghc

module IterativeLearn where

import Clingo hiding (readArgs)
import qualified While
import Logic

import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Applicative

import Data.List
import Data.Maybe
import Data.Char
import Data.Foldable (fold)
import Data.Functor.Identity

import System.IO
import System.Environment
import System.Exit

type Variable  = String
type Value     = Integer
type Feature   = String

type LineInstr   = Clingo.Fact
type Instruction = Clingo.Term

newtype Program = Program [LineInstr] deriving Show
newtype Input   = Input   [(Variable, Value)] deriving (Show, Eq, Ord)
newtype Output  = Output  [(Variable, Value)] deriving (Show, Eq, Ord)

data Limits = Limits{
    lmLineMax :: Integer }

data Example
  = UserExample{
        exID     :: String,
        exInput  :: Input,
        exOutput :: Output }
  | PostConditionExample {
        exID     :: String,
        exInput  :: Input }
  deriving Show

data Counterexample = Counterexample {
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
    cfInputVars       :: [Variable],
    cfOutputVars      :: [Variable],
    cfExtraVars       :: [Variable],
    cfLogicVars       :: [Variable],
    cfReadOnly        :: [Variable],
    cfDisallow        :: [Feature],
    cfExamples        :: [Example],
    cfPreCondition    :: Condition,
    cfPostCondition   :: Condition,
    cfConfFile        :: FilePath,
    cfThreads         :: Integer,
    cfEchoClingo      :: Bool,
    cfEchoASP         :: Bool,
    cfInteractive     :: Bool }
  deriving Show

defaultConf = Conf{
    cfIntRange        = error "int_range undefined",
    cfTimeMax         = error "time_max undefined",
    cfLineLimitMin    = 0,
    cfLineLimitStep   = 1,
    cfLineLimitMax    = Nothing,
    cfIfStatementsMax = Nothing,
    cfWhileLoopsMax   = Nothing,
    cfConstants       = [],
    cfInputVars       = [],
    cfOutputVars      = [],
    cfExtraVars       = [],
    cfLogicVars       = [],
    cfReadOnly        = [],
    cfDisallow        = [],
    cfExamples        = [],
    cfPreCondition    = "",
    cfPostCondition   = "",
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
  = (readConfFacts' conf facts){ cfExamples=examples }
  where
    ins = [(e, v, c)
        | Fact (Name "in") [TFun(Name e)[], TFun(Name v)[], TInt c] <- facts]
    outs = [(e, v, c)
        | Fact (Name "out") [TFun(Name e)[], TFun(Name v)[], TInt c] <- facts]
    examples = [UserExample{
        exID     = e,
        exInput  = Input  [(v,c) | (e',v,c) <- ins,  e == e'],
        exOutput = Output [(v,c) | (e',v,c) <- outs, e == e'] }
        | e <- nub . sort $ [e | (e,_,_) <- ins ++ outs]]
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
        Fact (Name "input_variable") [TFun (Name var) []] ->
            readConfFacts' (conf{ cfInputVars=var : cfInputVars conf }) facts
        Fact (Name "output_variable") [TFun (Name var) []] ->
            readConfFacts' (conf{ cfOutputVars=var : cfOutputVars conf }) facts
        Fact (Name "extra_variable") [TFun (Name var) []] ->
            readConfFacts' (conf{ cfExtraVars=var : cfExtraVars conf }) facts
        Fact (Name "read_only_variable") [TFun (Name var) []] ->
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
        _ ->
            readConfFacts' conf facts
    readConfFacts' conf [] = conf

--------------------------------------------------------------------------------
main = do
    args <- getArgs
    let ([confPath], conf) = readArgs args defaultConf
    conf <- readConfFile confPath conf
    void $ iterativeLearnConf conf

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
    putStrLn $ "Searching for a program with " ++ linesString
            ++ " lines satisfying " ++ show (length exs) ++ " example(s)..."

    mProg <- findProgramConcurrent exs limss conf

    case mProg of
      Just prog -> do
        putStrLn "Found the following program:"
        printProgram prog
        conf <- interactivePause conf
        let lineMax = programLength prog
        case cfPostCondition conf of
            _:_ -> iterativeLearn' prog exs lims{lmLineMax = lineMax} conf
            ""  -> return prog
      Nothing -> do
        let lims' = last limss
        let lineMax = lmLineMax lims' + cfLineLimitStep conf
        case cfLineLimitMax conf of
            Just limit | lineMax > limit -> do
                putStrLn $ "Failure: no such program can be found within the"
                        ++ " configured line limits."
                exitFailure
            _ -> do
                putStrLn "No such program found."
                iterativeLearn exs (lims'{lmLineMax=lineMax}) conf

iterativeLearn' :: Program -> [Example] -> Limits -> Conf -> IO Program
iterativeLearn' prog exs lims conf = do
    putStrLn "Searching for a counterexample to falsify the postcondition..."
    mCounter <- findCounterexample prog conf
    case mCounter of
        Just cex -> do
            let Counterexample{
                ceInput          = Input input,
                ceActualOutput   = Output actual,
                ceExpectedOutput = expected } = cex

            let deficient = null expected
            case deficient of
                True  -> do
                    putStrLn $ "Failure: counterexamples were found, but"
                            ++ " none admitting a solution the postcondition."
                            ++ " Possible causes:"
                    putStrLn $ "- The postcondition is unsatisfiable for some"
                            ++ " valid input."
                    putStrLn $ "- The precondition is satisfiable for some"
                            ++ " invalid input."
                    putStrLn $ "- The given int_range is too small."
                False -> do
                    putStrLn "Found the following counterexample:"

            putStrLn $ "   Input:    " ++ (intercalate ", "
                     $ [v++"="++show c | (v,c) <- sort input ])
            putStrLn $ "   Expected: " ++ case expected of {
                [] -> "(none)";
                _  -> intercalate " | " $ [
                    intercalate ", " [v ++ "=" ++ show c | (v,c) <- os]
                    | Output os <- take 5 $ sort [
                        Output (sort os) | Output os <- expected]]
                    ++ if (length expected > 5) then ["..."] else [] }
            putStrLn $ "   Output:   " ++ case actual of {
                [] -> "(none)";
                _  -> intercalate ", " [v++"="++show c | (v,c) <- sort actual ] }

            when deficient exitFailure
            conf <- interactivePause conf

            let ex = PostConditionExample{
                exID     = head $ map (("cx"++) . show) [1..] \\ map exID exs,
                exInput  = Input input }
            iterativeLearn (ex : exs) lims conf
        Nothing -> do
            putStrLn "Success: the postcondition could not be falsified."
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
        CRUnsatisfiable ->
            return Nothing

findProgramConcurrent :: [Example] -> [Limits] -> Conf -> IO (Maybe Program)
findProgramConcurrent exs limss conf = do
    tasks <- forM limss $ \lims -> do
        result <- newEmptyMVar
        thread <- forkIO $ putMVar result =<< findProgram exs lims conf
        return (thread, result)
    results <- forM (zip tasks [0..]) $ \((thread, resultMVar), i) -> do
        prior <- forM (take i tasks) $ \(_,r) -> do
            empty <- isEmptyMVar r
            if empty then return Nothing else readMVar r
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
            ["con(" ++ intercalate "; " (map show constants) ++ ")."]) ++
        (guard (not $ null allVars) >>
            ["var(" ++ intercalate "; " (allVars \\ logicVars) ++ ")."]) ++
        (guard (not $ null allVars) >>
            ["write_var(" ++ intercalate "; " (allVars \\ readOnly) ++  ")."]) ++
        (guard (not $ null disallow) >>
            ["disallow(" ++ intercalate "; " disallow ++ ")."])

    exampleLines = do
        example <- exs
        let (id, Input inps) = (exID example, exInput example)
        let ins = do
            (var, val) <- inps
            return $ "in(" ++ id ++ "," ++ var ++ "," ++ show val ++ ")."
        let outs = do
            UserExample{ exOutput=Output outps } <- return example
            (var, val) <- outps
            return $ "out(" ++ id ++ "," ++ var ++ "," ++ show val ++ ")."
        return $ unwords $ ins ++ outs
       
    postCondLines =
        let ids = [id | PostConditionExample{ exID=id } <- exs ] in
        let iVars = [v | v <- inputVars, ("In_"++v) `isFreeIn` postCond] in
        let oVars = [v | v <- outputVars, ("Out_"++v) `isFreeIn` postCond] in
        let plVars = [v | v <- headMap toUpper <$> (logicVars \\ outputVars),
                          v `isFreeIn` postCond] in
        if (null ids) then [] else [
        "postcon_run(" ++ intercalate ";" ids ++ ").",
        "postcon("++ intercalate "," ("R" : map ("Out_"++) oVars) ++") :- "
        ++ "postcon_run(R), "
        ++ concat ["int(Out_"++ v ++"), " | v <- oVars]
        ++ concat ["int("++ v ++"), " | v <- plVars]
        ++ concat ["in(R,"++v++",In_"++v++"), " | v <- iVars]
        ++ postCond ++".",
        ":- postcon_run(R), "
        ++ concat ["run_var_out(R,"++v++",Out_"++v++"), " | v <- oVars]
        ++ "not postcon("++ intercalate "," ("R" : map ("Out_"++) oVars) ++")."]

    allVars = nub . sort $ inputVars ++ outputVars ++ extraVars
    
    Conf{ cfTimeMax        =timeMax,   cfIntRange     =(intMin, intMax),
          cfInputVars      =inputVars, cfOutputVars   =outputVars,
          cfExtraVars      =extraVars, cfConstants    =constants,
          cfDisallow       =disallow,  cfReadOnly     =readOnly,
          cfIfStatementsMax=ifMax,     cfWhileLoopsMax=whileMax,
          cfLogicVars      =logicVars, cfPostCondition=postCond} = conf
    Limits{ lmLineMax=lineMax } = lims
    
--------------------------------------------------------------------------------
findCounterexample :: Program -> Conf -> IO (Maybe Counterexample)
findCounterexample prog conf = do
    let code = findCounterexampleASP prog conf
    result <- runClingoConf [CICode code] Nothing conf
    case result of
        CRSatisfiable (answer : _) -> do
            let inputs = do
                Fact (Name "counter_in") args <- answer
                let [TFun (Name name) [], TInt value] = args
                guard $ name `elem` cfInputVars conf
                return (name, value)
            let outputs = do
                Fact (Name "counter_out") args <- answer
                [TFun (Name name) [], TInt value] <- [args]
                guard $ name `elem` cfOutputVars conf
                return (name, value)
            let expected = do
                Fact (Name "postcon") args <- answer
                let names = [v | v <- outputVars,
                             ("Out_"++v) `isFreeIn` postCond]
                let values = [c | TInt c <- args]
                return $ Output (zip names values)
            return $ Just $ Counterexample{
                ceInput          = Input inputs,
                ceActualOutput   = Output outputs,
                ceExpectedOutput = expected }
        CRUnsatisfiable ->
            return Nothing
  where
    Conf{ cfPostCondition=postCond, cfOutputVars=outputVars } = conf

findCounterexampleASP :: Program -> Conf -> String
findCounterexampleASP prog conf
  = unlines . intercalate [""] $ [
        headerLines, programLines, preCondLines, postCondLines]
  where
    headerLines = [
        "#const time_max=" ++ show timeMax ++ ".",
        "#const int_min=" ++ show intMin ++ ".",
        "#const int_max=" ++ show intMax ++ ".",
        "#include \"counterexample.lp\".",      
        "input_var("  ++ intercalate "; " inputVars  ++ ").",
        "output_var(" ++ intercalate "; " outputVars ++ ")."]

    programLines =
        let Program instrs = prog in Clingo.showFactLines instrs

    preCondLines =
        let preVars = filter ((`isFreeIn` preCond) . ("In_"++)) inputVars in
        let inDom   = ["counter_in("++v++", In_"++v++")" | v<-preVars] in [
        let preConds = filter (not . null) [preCond] in
        "precon :- "++ intercalate ", " (preConds ++ inDom) ++".",
        ":- not precon."]
    
    postCondLines =
        let inVars = filter ((`isFreeIn` postCond) . ("In_"++)) inputVars in
        let inDom = ["counter_in("++v++", In_"++v++")" | v<-inVars] in
        let plVars = [v | v <- headMap toUpper <$> (logicVars \\ outputVars),
                          v `isFreeIn` postCond] in
        let expOutDom = ["int(Out_"++ v ++")" | v <- outputVars]
                     ++ ["int("++ v ++")" | v <- plVars] in
        let postConds = filter (not . null) [postCond] in
        let ruleBody = intercalate ", " (postConds ++ inDom ++ expOutDom) in
        let outVars = filter ((`isFreeIn` postCond) . ("Out_"++)) outputVars in
        let args = case outVars of {
            [] -> "";
            _  -> "(" ++ intercalate ", " (map ("Out_"++) outVars) ++ ")" } in
        let actOutDom = ["counter_out("++v++", Out_"++v++")" | v<-outVars]
                     ++ ["int("++ v ++")" | v <- plVars] in
        let consHead = "postcon" ++ args in [
        "postcon"++ args ++" :- "++ ruleBody ++".",
        ":- " ++ intercalate ", " (consHead : actOutDom) ++ ".",
        "any_postcon :- postcon(" ++ intercalate "," ("_" <$ outVars) ++ ").",
        "#show postcon/" ++ show (length outVars) ++ "."]

    Conf{ cfIntRange=(intMin, intMax), cfTimeMax=timeMax,
          cfInputVars=inputVars, cfOutputVars=outputVars,
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
showInteractiveHelp = do
    putStrLn $ "Type enter to continue, c to exit interactive mode,"
            ++ " or q to quit."

--------------------------------------------------------------------------------
runClingoConf :: [ClingoInput] -> Maybe String -> Conf -> IO ClingoResult
runClingoConf input maybeRunID conf = do
    let options = runClingoOptions{
        rcEchoStdout = cfEchoClingo conf,
        rcEchoInput  = cfEchoASP conf,
        rcIdentifier = maybeRunID }
    runClingo options (input ++ [CIFile $ cfConfFile conf])

showProgram :: Program -> [String]
showProgram (Program [])
  = ["   (empty program)"]
showProgram (Program facts)
  = While.showProgram . catMaybes . map While.readLineInstr $ facts

printProgram :: Program -> IO ()
printProgram prog
  = mapM_ putStrLn $ showProgram prog

programLength :: Program -> Integer
programLength (Program facts)
  = genericLength . catMaybes . map While.readLineInstr $ facts

--------------------------------------------------------------------------------
traverseFreeVariables
  :: Applicative f => (Variable -> f Variable) -> Condition -> f Condition
traverseFreeVariables act cond@(c : _) | isVariableHead c
  = (++) <$> act var <*> traverseFreeVariables act tail
  where (var, tail) = span isVariableBody cond
traverseFreeVariables act cond@(_ : _)
  = (head ++) <$> traverseFreeVariables act tail
  where (head, tail) = break isVariableHead cond
traverseFreeVariables _ ""
  = pure ""

isVariableHead c = isUpper c
isVariableBody c = isAlphaNum c || c == '_'

mapFreeVariables :: (Variable -> Variable) -> Condition -> Condition
mapFreeVariables f cond 
  = runIdentity $ traverseFreeVariables (pure . f) cond

freeVariables :: Condition -> [Variable]
freeVariables cond
  = nub . sort . getConst $ traverseFreeVariables (\v -> Const [v]) cond

isFreeIn :: Variable -> Condition -> Bool
isFreeIn var = elem var . freeVariables

headMap :: (a -> a) -> [a] -> [a]
headMap f (x : xs) = f x : xs
headMap _ []       = []
