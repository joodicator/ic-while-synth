#!/usr/bin/env runghc

import Clingo hiding (readArgs)
import qualified While

import Data.List
import Data.Maybe
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import System.IO
import System.Environment
import System.Exit

type Variable  = String
type Value     = Integer
type Condition = String
type Feature   = String

newtype Program  = Program [Clingo.Fact] deriving Show
newtype Input    = Input   [(Variable, Value)] deriving Show
newtype Output   = Output  [(Variable, Value)] deriving Show

data Conditions = Conditions{
    cnPreCondition  :: Condition,
    cnPostCondition :: Condition }

data Limits = Limits{
    lmLineMax :: Integer }

data Example = Example{
    exID     :: String,
    exInput  :: Input,
    exOutput :: Output }
  deriving Show

data Counterexample = Counterexample {
    ceInput          :: Input,
    ceActualOutput   :: Output,
    ceExpectedOutput :: Output }
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
    cfReadOnly        :: [Variable],
    cfDisallow        :: [Feature],
    cfExamples        :: [Example],
    cfPreCondition    :: Condition,
    cfPostCondition   :: Condition,
    cfConfFile        :: FilePath,
    cfEchoClingo      :: Bool,
    cfEchoASP         :: Bool,
    cfThreads         :: Integer }
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
    cfReadOnly        = [],
    cfDisallow        = [],
    cfExamples        = [],
    cfPreCondition    = "",
    cfPostCondition   = "",
    cfConfFile        = undefined,
    cfEchoClingo      = False,
    cfEchoASP         = False,
    cfThreads         = 1 }

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
    examples = [Example{
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
        _ ->
            readConfFacts' conf facts
    readConfFacts' conf [] = conf

--------------------------------------------------------------------------------
main = do
    args <- getArgs
    let ([confPath], conf) = readArgs args defaultConf
    conf <- readConfFile confPath conf

    let examples = cfExamples conf
    let limits = Limits{ lmLineMax = cfLineLimitMin conf }

    iterativeLearn examples limits conf

readArgs :: [String] -> Conf -> ([String], Conf)
readArgs (arg : args) conf | "-" `isPrefixOf` arg
  = case arg of
      "-j" ->
        let param : args' = args in
        readArgs args' (conf { cfThreads = read param })
      _  ->
        error $ "Unrecognised option: " ++ arg
readArgs (arg : args) conf | otherwise
  = let (args', conf') = readArgs args conf in (arg:args', conf')
readArgs [] conf
  = ([], conf)

--------------------------------------------------------------------------------
iterativeLearn :: [Example] -> Limits -> Conf -> IO ()
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
        let lineMax = programLength prog
        case cfPostCondition conf of
            _:_ -> iterativeLearn' prog exs lims{lmLineMax = lineMax} conf
            ""  -> exitSuccess
      Nothing -> do
        let lims' = last limss
        let lineMax = lmLineMax lims' + cfLineLimitStep conf
        case cfLineLimitMax conf of
            Just limit | lineMax > limit -> do
                putStrLn $ "Failure: no such program can be found within the"
                        ++ " configured line limits."
            _ -> do
                putStrLn "No such program found."
                iterativeLearn exs (lims'{lmLineMax=lineMax}) conf
        

iterativeLearn' :: Program -> [Example] -> Limits -> Conf -> IO ()
iterativeLearn' prog exs lims conf = do
    putStrLn "Searching for a counterexample to falsify the postcondition..."
    let conds = Conditions{
        cnPreCondition  = cfPreCondition conf,
        cnPostCondition = cfPostCondition conf}
    mCounter <- findCounterexample prog conds conf
    case mCounter of
        Just cex -> do
            let Counterexample{
                ceInput          = Input input,
                ceActualOutput   = Output actual,
                ceExpectedOutput = Output expected } = cex

            let deficient = not . null $ map fst actual \\ map fst expected
            case deficient of
                True  -> do
                    putStrLn $ "Failure: counterexamples were found, but"
                            ++ " none admitting a solution the postcondition."
                            ++ " Possible causes:"
                    putStrLn $ "   - The postcondition is unsatisfiable for some"
                            ++ " valid input."
                    putStrLn $ "   - The given int_range is too small."
                False -> putStrLn "Found the following counterexample:"

            putStrLn $ "   Input:    " ++ (intercalate ", "
                     $ [v++" = "++show c | (v,c) <- input ])
            putStrLn $ "   Expected: " ++ case expected of {
                [] -> "(none)";
                _ -> intercalate ", " [v++" = "++show c | (v,c) <- expected ] }
            putStrLn $ "   Output:   " ++ case actual of {
                [] -> "(none)";
                _  -> intercalate ", " [v++" = "++show c | (v,c) <- actual ] }

            when deficient exitFailure

            let ex = Example{
                exID     = head $ map (("cx"++) . show) [1..] \\ map exID exs,
                exInput  = Input input,
                exOutput = Output expected }
            iterativeLearn (ex : exs) lims conf
        Nothing -> do
            putStrLn "Success: the postcondition could not be falsified."

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
  = unlines . intercalate [""] $ [headerLines, biasLines, exampleLines]
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
            ["var(" ++ intercalate "; " allVars ++ ")."]) ++
        (guard (not $ null allVars) >>
            ["write_var(" ++ intercalate "; " (allVars \\ readOnly) ++  ")."]) ++
        (guard (not $ null disallow) >>
            ["disallow(" ++ intercalate "; " disallow ++ ")."])

    exampleLines = do
        Example{ exID=id, exInput=Input inps, exOutput=Output outps } <- exs
        let ins = do
            (var, val) <- inps
            return $ "in(" ++ id ++ "," ++ var ++ "," ++ show val ++ ")."
        let outs = do
            (var, val) <- outps
            return $ "out(" ++ id ++ "," ++ var ++ "," ++ show val ++ ")."
        return $ intercalate " " (ins ++ outs)
    
    allVars = nub . sort $ inputVars ++ outputVars ++ extraVars
    
    Conf{ cfTimeMax        =timeMax,   cfIntRange     =(intMin, intMax),
          cfInputVars      =inputVars, cfOutputVars   =outputVars,
          cfExtraVars      =extraVars, cfConstants    =constants,
          cfDisallow       =disallow,  cfReadOnly     =readOnly,
          cfIfStatementsMax=ifMax,     cfWhileLoopsMax=whileMax} = conf
    Limits{ lmLineMax=lineMax } = lims
    
--------------------------------------------------------------------------------
findCounterexample :: Program -> Conditions -> Conf -> IO (Maybe Counterexample)
findCounterexample prog conds conf = do
    let code = findCounterexampleASP prog conds conf
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
                Fact (Name "counter_expected_out") args <- answer
                let [TFun (Name name) [], TInt value] = args
                guard $ name `elem` cfOutputVars conf
                return (name, value)
            return $ Just $ Counterexample{
                ceInput          = Input inputs,
                ceActualOutput   = Output outputs,
                ceExpectedOutput = Output expected }
        CRUnsatisfiable ->
            return Nothing

findCounterexampleASP :: Program -> Conditions -> Conf -> String
findCounterexampleASP prog conds conf
  = unlines . intercalate [""] $ [
        headerLines, programLines, preCondLines, postCondLines, expectedLines]
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
        let preVars = filter ((`isInfixOf` preCond) . ("In_"++)) inputVars in
        let inDom   = ["counter_in("++v++", In_"++v++")" | v<-preVars] in [
        let preConds = filter (not . null) [preCond] in
        "precon :- "++ intercalate ", " (preConds ++ inDom) ++".",
        ":- not precon."]
    
    postCondLines =
        let inVars = filter ((`isInfixOf` postCond) . ("In_"++)) inputVars in
        let outVars = filter ((`isInfixOf` postCond) . ("Out_"++)) outputVars in
        let args = case outVars of {
            [] -> "";
            _  -> "(" ++ intercalate ", " (map ("Out_"++) outVars) ++ ")" } in
        let inDom = ["counter_in("++v++", In_"++v++")" | v<-inVars] in
        let actOutDom = ["counter_out("++v++", Out_"++v++")" | v<-outVars] in
        let expOutDom = ["int(Out_"++v++")" | v<-outVars] in
        let postConds = filter (not . null) [postCond] in
        let ruleBody = intercalate ", " (postConds ++ inDom ++ expOutDom) in
        let consHead = "postcon" ++ args in [
        "postcon"++ args ++" :- "++ ruleBody ++".",
        ":- " ++ intercalate ", " (consHead : actOutDom) ++ "."]

    expectedLines = do
        let outVars = filter ((`isInfixOf` postCond) . ("Out_"++)) outputVars
        v <- outVars
        let args = [if v == v' then "Out_"++v else "_" | v' <- outVars]
        let body = "postcon("++ intercalate ", " args ++")"
        return $ "counter_expected_out("++v++", Out_"++v++") :- "++body++"."

    Conf{ cfIntRange=(intMin, intMax), cfTimeMax=timeMax,
          cfInputVars=inputVars, cfOutputVars=outputVars } = conf
    Conditions{ cnPreCondition = preCond,  cnPostCondition = postCond } = conds

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
