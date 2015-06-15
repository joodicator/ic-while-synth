import Clingo
import qualified While

import Data.List
import Data.Maybe
import Control.Monad
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
    cfIntRange      :: (Integer, Integer),
    cfTimeMax       :: Integer, 
    cfLineLimitMin  :: Integer,
    cfLineLimitStep :: Integer,
    cfLineLimitMax  :: Maybe Integer,
    cfConstants     :: [Value],   
    cfInputVars     :: [Variable],
    cfOutputVars    :: [Variable],
    cfExtraVars     :: [Variable],
    cfReadOnly      :: [Variable],
    cfDisallow      :: [Feature],
    cfExamples      :: [Example],
    cfPreCondition  :: Condition,
    cfPostCondition :: Condition,
    cfEchoClingo    :: Bool,
    cfEchoASP       :: Bool }
  deriving Show

defaultConf = Conf{
    cfIntRange      = error "int_range undefined",
    cfTimeMax       = error "time_max undefined",
    cfLineLimitMin  = 0,
    cfLineLimitStep = 1,
    cfLineLimitMax  = Nothing,
    cfConstants     = [],
    cfInputVars     = [],
    cfOutputVars    = [],
    cfExtraVars     = [],
    cfReadOnly      = [],
    cfDisallow      = [],
    cfExamples      = [],
    cfPreCondition  = "",
    cfPostCondition = "",
    cfEchoClingo    = False,
    cfEchoASP       = False }

readConfFile :: FilePath -> IO Conf
readConfFile filePath = do
    let options = runClingoOptions{ rcEchoStdout=False }
    result <- runClingo options [CIFile filePath]
    case result of
        CRSatisfiable (answer:_) -> return $ readConfFacts answer
        CRUnsatisfiable          -> error $ filePath ++ " unsatisfiable."

readConfFacts :: [Fact] -> Conf
readConfFacts facts = conf where
    conf = (readConfFacts' defaultConf facts){ cfExamples=examples }
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
        Fact (Name name) _ | name `elem` ["in", "out"] ->
            readConfFacts' conf facts
        fact ->
            error $ "unrecognised configuration entry: " ++ showFact fact
    readConfFacts' conf [] = conf

--------------------------------------------------------------------------------
main = do
    confPath:_ <- getArgs
    conf <- readConfFile confPath
    let examples = cfExamples conf
    let limits = Limits{ lmLineMax = cfLineLimitMin conf }
    iterativeLearn examples limits conf

iterativeLearn :: [Example] -> Limits -> Conf -> IO ()
iterativeLearn exs lims conf = do
    putStrLn $ "Searching for a program with at most " ++ show (lmLineMax lims)
            ++ " lines that satisfies " ++ show (length exs) ++ " known"
            ++ " example(s)..."
    mProg <- findProgram exs lims conf
    case mProg of
      Just prog -> do
        printProgram prog
        iterativeLearn' prog exs lims conf
      Nothing -> do
        let lineMax = lmLineMax lims + cfLineLimitStep conf
        case cfLineLimitMax conf of
            Just limit | lineMax > limit ->
                putStrLn ("Failure: no such program can be found within the"
                       ++ "configured line limits.")
            _  ->
                iterativeLearn exs (lims{lmLineMax=lineMax}) conf
        

iterativeLearn' :: Program -> [Example] -> Limits -> Conf -> IO ()
iterativeLearn' prog exs lims conf = do
    let conds = Conditions{
        cnPreCondition  = cfPreCondition conf,
        cnPostCondition = cfPostCondition conf}
    mCounter <- findCounterexample prog conds conf
    case mCounter of
        Just cex -> do
            let Counterexample{
                ceInput          = input,
                ceActualOutput   = actual,
                ceExpectedOutput = expected } = cex
            putStrLn "Counter-example found:"
            putStrLn $ "   Input:    " ++ intercalate ", " [
                v++" = "++show c | let Input xs = input, (v,c) <- xs ]
            putStrLn $ "   Output:   " ++ case actual of {
                Output [] -> "(none)";
                Output xs -> intercalate ", " [v++" = "++show c | (v,c) <- xs ] }
            putStrLn $ "   Expected: " ++ intercalate ", " [
                v++" = "++show c | let Output xs = expected, (v,c) <- xs ]
            let ex = Example{
                exID     = head $ map (("cex"++) . show) [1..] \\ map exID exs,
                exInput  = input,
                exOutput = expected }
            iterativeLearn (ex : exs) lims conf
        Nothing ->
            putStrLn ("Solution complete: no counter-example can be found to"
                  ++ " falsify the postcondition.")

--------------------------------------------------------------------------------
findProgram :: [Example] -> Limits -> Conf -> IO (Maybe Program)
findProgram exs lims conf = do
    let code = findProgramASP exs lims conf
    let options = runClingoOptions{
        rcEchoStdout = cfEchoClingo conf,
        rcEchoInput  = cfEchoASP conf }
    result <- runClingo options [CICode code]
    case result of
        CRSatisfiable (answer:_) ->
            return $ Just (Program answer)
        CRUnsatisfiable ->
            return Nothing

findProgramASP :: [Example] -> Limits -> Conf -> String
findProgramASP exs lims conf
  = unlines . intercalate [""] $ [headerLines, biasLines, exampleLines]
  where
    headerLines = [
        "#const line_max=" ++ show lineMax ++ ".",
        "#const time_max=" ++ show timeMax ++ ".",
        "#const int_min=" ++ show intMin ++ ".",
        "#const int_max=" ++ show intMax ++ ".",
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
    
    Conf{ cfTimeMax  =timeMax,   cfIntRange  =(intMin, intMax),
          cfInputVars=inputVars, cfOutputVars=outputVars,
          cfExtraVars=extraVars, cfConstants =constants,
          cfDisallow =disallow,  cfReadOnly  =readOnly } = conf
    Limits{ lmLineMax=lineMax } = lims
    
--------------------------------------------------------------------------------
findCounterexample :: Program -> Conditions -> Conf -> IO (Maybe Counterexample)
findCounterexample prog conds conf = do
    let code = findCounterexampleASP prog conds conf
    let options = runClingoOptions{
        rcEchoStdout = cfEchoClingo conf,
        rcEchoInput  = cfEchoASP conf }
    result <- runClingo options [CICode code]
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
showProgram :: Program -> [String]
showProgram (Program [])
  = ["   (empty program)"]
showProgram (Program facts)
  = While.showProgram . catMaybes . map While.readLineInstr $ facts

printProgram :: Program -> IO ()
printProgram prog
  = mapM_ putStrLn $ showProgram prog
