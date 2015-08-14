{-# LANGUAGE OverloadedStrings, ViewPatterns, TransformListComp,
             TypeSynonymInstances, FlexibleInstances #-}

module IterativeLearn.Base where

import Control.Applicative
import Control.Monad
import Control.Concurrent
import qualified Control.Concurrent.MSem as MSem

import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.Char
import Data.MonoTraversable
import Data.String
import Data.Monoid
import Data.Ratio

import System.IO
import System.Exit

import GHC.Exts

import Control.Monad.Catch
import Control.Monad.IO.Class

import qualified Logic
import qualified ASP
import qualified ASPString
import qualified While
import qualified Abstract.Base as Abstract
import Abstract.Util
import Util
import Clingo hiding (readArgs)

--------------------------------------------------------------------------------
type Value   = Integer
type Feature = String

type ArrayName  = String
type ArraySize  = Integer
type Variable   = String
type Subroutine = String

type LineInstr   = Clingo.Fact
type Instruction = Clingo.Term

type ArraySizes = M.Map ArrayName ArraySize
type HsBindings = [(HsVar, HsInput)]
type ASPProp    = Logic.Prop ASP.Literal

newtype Program
  = Program [LineInstr]
  deriving Show

newtype Input
  = Input{ unInput :: [(WhileVar, Value)] }
  deriving (Show, Eq, Ord)

newtype Output
  = Output{ unOutput :: [(WhileVar, Value)] }
  deriving (Show, Eq, Ord)

data Direction
  = In | Out

data WhileVar
  = VScalar Variable
  | VArray ArrayName Integer
  deriving (Eq, Ord)

data Condition
  = CondASPString ASPString.Condition
  | CondASPProp   [(ArraySizes, ASPProp)]
  | CondEmpty
  deriving Show

data Example
  = UserExample{
        exID     :: String,
        exArrays :: ArraySizes,
        exInput  :: Input,
        exOutput :: Output }
  | PostConditionExample {
        exID     :: String,
        exArrays :: ArraySizes,
        exInput  :: Input }
  deriving Show

data Counterexample = Counterexample {
    ceArrays         :: ArraySizes,
    ceInput          :: Input,
    ceActualOutput   :: Maybe Output,
    ceExpectedOutput :: [Output] }
  deriving Show

--------------------------------------------------------------------------------
iPre, oPre :: String
iPre = "In_"
oPre = "Out_"

--------------------------------------------------------------------------------
data Conf = Conf{
    cfIntRange        :: (Integer, Integer),
    cfTimeMax         :: Integer, 
    cfStackMax        :: Integer,
    cfIfStatementsMax :: Maybe Integer,
    cfWhileLoopsMax   :: Maybe Integer,
    cfConstants       :: [Value],   
    cfDisallow        :: [Feature],

    cfLineLimitMin    :: [(Subroutine, Integer)],
    cfLineLimitStep   :: [(Subroutine, Integer)],
    cfLineLimitMax    :: [(Subroutine, Integer)],
    cfPresetLines     :: [LineInstr],

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
    cfPutLines        :: [String] -> IO (),
    cfPrintTime       :: Bool }

defaultConf :: Conf
defaultConf = Conf{
    cfIntRange        = error "int_range undefined",
    cfTimeMax         = error "time_max undefined",
    cfStackMax        = 0,
    cfIfStatementsMax = Nothing,
    cfWhileLoopsMax   = Nothing,
    cfConstants       = [],
    cfDisallow        = [],

    cfLineLimitMin    = [],
    cfLineLimitStep   = [],
    cfLineLimitMax    = [],
    cfPresetLines     = [],

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
    cfPreCondition    = CondEmpty,
    cfPostCondition   = CondEmpty,

    cfConfFile        = undefined,
    cfThreads         = 1,
    cfEchoClingo      = False,
    cfEchoASP         = False,
    cfInteractive     = False,
    cfPutLines        = mapM_ putStrLn,
    cfPrintTime       = False }

readConfFile :: FilePath -> Conf -> IO Conf
readConfFile filePath conf = do
    let options = runClingoOptions{ rcEchoStdout=False }
    let code = "#const line_max=0."
    result <- runClingo options [CICode code, CIFile filePath]
    case result of
        CRSatisfiable (answer:_) -> do
            conf' <- runHaskellT $ readConfFacts answer conf
            return $ conf'{ cfConfFile=filePath }
        CRUnsatisfiable ->
            error $ filePath ++ " unsatisfiable."
        _ -> error "impossible"

readConfFacts :: [Fact] -> Conf -> HaskellT IO Conf
readConfFacts inFacts inConf
  = return inConf >>= readConfFacts'   `flip` inFacts
                  >>= readConfFacts''  `flip` inFacts
                  >>= readConfFacts''' `flip` inFacts
  where    
    readConfFacts' conf (fact : facts) = case fact of
        Fact (Name "int_range") [TInt imin, TInt imax] ->
            readConfFacts' (conf{ cfIntRange=(imin, imax) }) facts
        Fact (Name "time_limit") [TInt tmax] ->
            readConfFacts' (conf{ cfTimeMax=tmax }) facts
        Fact (Name "stack_limit") [TInt smax] ->
            readConfFacts' (conf{ cfStackMax=smax }) facts
        Fact (Name "line_limit_min") args -> readConfFacts' `flip` facts $ case args of
            [TFun (Name sub) [], TInt lmin] ->
                conf{ cfLineLimitMin=(sub,lmin):cfLineLimitMin conf }
            [TInt lmin] ->
                conf{ cfLineLimitMin=("main",lmin):cfLineLimitMin conf }
            _  -> error $ "Invalid argument format: " ++ show fact
        Fact (Name "line_limit_step") args -> readConfFacts' `flip` facts $ case args of
            [TFun (Name sub) [], TInt lstep] ->
                conf{ cfLineLimitStep=(sub,lstep):cfLineLimitStep conf }
            [TInt lstep] ->
                conf{ cfLineLimitStep=("main",lstep):cfLineLimitStep conf }
            _ -> error $ "Invalid argument format: " ++ show fact
        Fact (Name "line_limit_max") args -> readConfFacts' `flip` facts $ case args of
            [TFun (Name sub) [], TInt lmax] ->
                conf{ cfLineLimitMax=(sub,lmax):cfLineLimitMax conf }
            [TInt lmax] ->
                conf{ cfLineLimitMax=("main",lmax):cfLineLimitMax conf }
            _ -> error $ "Invalid argument format: " ++ show fact
        Fact (Name "preset_line_instr") [lineNum, instr] ->
            let line = TFun "" [TFun "main" [], lineNum] in readConfFacts' (conf{
                cfPresetLines = Fact "preset_sub_line_instr" [line, instr]
                              : cfPresetLines conf }) facts
        Fact (Name "preset_sub_line_instr") [line, instr] ->
            readConfFacts' (conf { cfPresetLines=fact : cfPresetLines conf }) facts
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
        _ -> readConfFacts' conf facts
    readConfFacts' conf [] = return conf

    readConfFacts'' conf (fact : facts) = case fact of
        Fact (Name "precondition") [condTerm] ->
            readCondTerm condTerm conf >>= \mCond -> case mCond of
                Just cond -> readConfFacts'' (conf{ cfPreCondition=cond }) facts
                Nothing   -> error $ "Invalid precondition: "++ showTerm condTerm
        Fact (Name "postcondition") [condTerm] -> do
            readCondTerm condTerm conf >>= \mCond -> case mCond of
                Just cond -> readConfFacts'' (conf{ cfPostCondition=cond }) facts
                Nothing   -> error $ "Invalid postcondition: "++ showTerm condTerm
        _ -> readConfFacts'' conf facts
    readConfFacts'' conf [] = return conf

    readConfFacts''' conf facts
      = return $ conf{ cfExamples=examples }
      where
        xArrays = [(e, a, l)
            | Fact "array" [TFun(Name e)[], TFun(Name a)[], TInt l] <- facts]
        ins = [(e, v, c)
            | Fact (Name "in") [TFun(Name e)[], vt, TInt c] <- facts,
              v <- maybeToList $ readVarTerm vt]
        outs = [(e, v, c)
            | Fact (Name "out") [TFun(Name e)[], vt, TInt c] <- facts,
              v <- maybeToList $ readVarTerm vt]
        examples = [UserExample{
            exID     = e,
            exArrays = M.fromList [(a,l) | (e',a,l) <- xArrays, e == e'],
            exInput  = Input  [(v,c) | (e',v,c) <- ins,  e == e'],
            exOutput = Output [(v,c) | (e',v,c) <- outs, e == e'] }
            | e <- nub . sort $ [e | (e,_,_) <- ins ++ outs]
                              ++[e | (e,_,_) <- xArrays]]

--------------------------------------------------------------------------------
iterativeLearnConf :: Conf -> IO Program
iterativeLearnConf conf = do
    path <- case iterSubs of
      _ | not . null $ allSubs \\ (iterSubs1 ++ constSubs1) -> do
        cfPutLines conf ["Error: it is required that at most one"
          ++ "subroutine lacks a line_limit_max/2 declaration; however, the"
          ++ "following subroutines lack one: "
          ++ intercalate "," (allSubs \\ constSubs1) ++"."]
        exitFailure
      (sub, lMin, lStep, Nothing) : _ -> return
        [(sub, max 0 (l-lStep), l) : constSubs | l <- [lMin,lMin+lStep..]]
      (sub, lMin, lStep, Just lMax) : _ -> return
        [(sub, max 0 (l-lStep), l) : constSubs | l <- [lMin,lMin+lStep..lMax]]
      [] -> return
        [constSubs]

    iterativeLearn (cfExamples conf) path conf
  where
    iterSubs1 = [s | (s,_,_,_) <- iterSubs]
    iterSubs
      = [(sub, lMin, lStep, mlMax)
      | sub <- allSubs
      , let lMin  = fromMaybe 0 $ lookup sub lMins
      , let lStep = fromMaybe 1 $ lookup sub lSteps
      , let mlMax = lookup sub lMaxs
      , maybe True (> lMin) mlMax
      , then sortWith by case mlMax of
            Nothing   -> Left  (lStep, lMin)
            Just lMax -> Right ((lMax-lMin)%lStep)
      , then take 1]        

    constSubs1 = [s | (s,_,_) <- constSubs]
    constSubs
      = [(sub, lMin, lMax)
      | sub <- allSubs
      , lMax <- maybeToList $ lookup sub lMaxs
      , let lMin = fromMaybe lMax $ lookup sub lMins
      , not $ elem sub iterSubs1]

    allSubs = map fst lMins ++ map fst lSteps ++ map fst lMaxs ++ [
        s | Fact _ [_, TFun "call" (TFun (Name s) [] : _)] <- cfPresetLines conf]

    Conf{ cfLineLimitMin=lMins, cfLineLimitStep=lSteps,
          cfLineLimitMax=lMaxs } = conf

type LineMin = Integer
type LineMax = Integer
type SearchNode = [(Subroutine, LineMin, LineMax)]
type SearchPath = [SearchNode]

iterativeLearn :: [Example] -> SearchPath -> Conf -> IO Program
iterativeLearn exs path conf = do
    let (nodes, path') = genericSplitAt (cfThreads conf) path

    case nodes of
        [] -> do
            cfPutLines conf [
                "Failure: no program exists satisfying the given constraints."]
            exitFailure
        _ | all null nodes -> do
            cfPutLines conf ["Searching for a program satisfying "
                          ++ show (length exs) ++ " example(s)..."]
        _ -> do
            cfPutLines conf ["Searching for a program with "++ noOxfordComma "and" [
                  rg ++ " '"++ sub ++"'"
                | sub <- nub . sort $ [s | n <- nodes, (s,_,_) <- n]
                , let ls = [l | n <- nodes, (s,_,l) <- n, s == sub]
                , let (mn,mx) = (minimum ls, maximum ls)
                , let rg = show mn ++ if mn==mx then "" else "-" ++ show mx
                ] ++ " line(s), satisfying "++ show (length exs) ++" example(s)..."]
    
    cont <- maybePrintTime conf $ do
        mProg <- case null exs of
            True  -> return . Just $ (head nodes, Program [])
            False -> findProgramConcurrent exs nodes conf
    
        case mProg of
          Just (node, prog) -> do
            cfPutLines conf $ ["Found the following program:"]
                           ++ showProgram prog
            return $ do
                _conf <- interactivePause conf
                let path'' = dropWhile (/= node) nodes ++ path'
                case cfPostCondition _conf of
                    CondEmpty -> return prog
                    _         -> iterativeLearn' prog exs path'' _conf
          Nothing -> do
            cfPutLines conf ["No such program found."]
            return $ iterativeLearn exs path' conf
    cont

iterativeLearn' :: Program -> [Example] -> SearchPath -> Conf -> IO Program
iterativeLearn' prog exs path conf = do
    cfPutLines conf [
        "Searching for a counterexample to falsify the postcondition..."]

    cont <- maybePrintTime conf $ do
        mCounter <- findCounterexample prog conf
        case mCounter of
            Just cex -> do
                let Counterexample{
                    ceArrays         = arrays,
                    ceInput          = Input input,
                    ceExpectedOutput = expected } = cex
    
                let deficient = null expected
                headLines <- return $ case deficient of
                    True  -> [
                        "Failure: counterexamples were found, but none admitting"
                        ++" a solution the postcondition. Possible causes:",
                        "- The postcondition is unsatisfiable for some valid"
                        ++" input.",
                        "- The precondition is satisfiable for some invalid"
                        ++" input.",
                        "- The given int_range is too small.",
                        "The counterexample in question is:"]
                    False -> [
                        "Found the following counterexample:"]
    
                let bodyLines = showCounterexample cex
                cfPutLines conf $ headLines ++ bodyLines

                return $ do
                    when deficient exitFailure
                    _conf <- interactivePause conf   
                    let ex = PostConditionExample{
                        exArrays = arrays,
                        exID     = head $ map (("cx"++) . show) [1::Int ..] \\ map exID exs,
                        exInput  = Input input }
                    iterativeLearn (ex : exs) path _conf
            Nothing -> do
                cfPutLines conf [
                    "The postcondition could not be falsified."]
                return $ return prog
    cont
    
--------------------------------------------------------------------------------
findProgram :: [Example] -> SearchNode -> Conf -> IO (Maybe Program)
findProgram exs node conf = do
    let code = findProgramASP exs node conf
    let maybeRunID = case cfThreads conf of {
        1 -> Nothing;
        _ -> Just $ intercalate "," [show l | (_,_,l) <- node] }
    result <- runClingoConf [CICode code] maybeRunID conf
    return $ case result of
        CRSatisfiable (answer:_) -> Just $ Program answer
        _                        -> Nothing

findProgramConcurrent
  :: [Example] -> [SearchNode] -> Conf -> IO (Maybe (SearchNode, Program))
findProgramConcurrent exs nodes conf = do
    outSem <- MSem.new (1 :: Integer)
    let conf' = conf{ cfPutLines = MSem.with outSem . cfPutLines conf }
    tasks <- forM nodes $ \node -> do
        result <- newEmptyMVar
        thread <- forkIO $ do
            prog <- findProgram exs node conf'
            putMVar result $ (,) node <$> prog
        return (thread, result)
    results <- forM (zip tasks [0..]) $ \((thread, resultMVar), i) -> do
        prior <- forM (take i tasks) $ \(_,r) -> do
            isEmpty <- isEmptyMVar r
            if isEmpty then return Nothing else readMVar r
        case any isJust prior of
          True  -> killThread thread >> return Nothing
          False -> readMVar resultMVar
    return (listToMaybe . catMaybes $ results)

findProgramASP :: [Example] -> SearchNode -> Conf -> String
findProgramASP exs node conf
  = unlines $ concat [headerLines, lineLines, biasLines, exampleLines, postCondLines]
  where
    headerLines = [
        "#const time_max=" ++ show timeMax ++ ".",
        "#const stack_max=" ++ show stackMax ++ ".",
        "#const int_min=" ++ show intMin ++ ".",
        "#const int_max=" ++ show intMax ++ ".",
        "#const if_max=" ++ maybe "any" show ifMax ++ ".",
        "#const while_max=" ++ maybe "any" show whileMax ++ ".",
        "#include \"learn.lp\"."]

    lineLines = [
        "sub_line_max("++ sub ++", "++ show lineMax ++")."
        | (sub, _, lineMax) <- node] ++ [
        "#const line_max="++ show lineMax ++ "."
        | ("main", _, lineMax) <- node]

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
            (name, size) <- M.toList $ exArrays example
            return $ "array("++ xID ++","++ name ++","++ show size ++")."
        ins <- return $ do
            (var, val) <- let Input is = exInput example in is
            return $ "in(" ++ xID ++ "," ++ varTerm var ++ "," ++ show val ++ ")."
        outs <- return $ do
            UserExample{ exOutput=Output outps } <- return example
            (var, val) <- outps
            return $ "out(" ++ xID ++ "," ++ varTerm var ++ "," ++ show val ++ ")."
        return $ unwords $ arrs ++ ins ++ outs
       
    postCondLines = case postCond of
      CondASPString postCondString ->
        let ids = [xid
                  | PostConditionExample{ exID=xid } <- exs ] in
        let iVars = [v
                    | v <- inputVars, (varVar' iPre v) `isFreeIn` postCond] in
        let oVars = [v
                    | v <- outputVars, (varVar' oPre v) `isFreeIn` postCond] in
        let plVars = [v
                     | v <- logicVars \\ outputVars, varVar v `isFreeIn` postCond] in
        if (null ids) then [] else [
        "postcon_run(" ++ intercalate ";" ids ++ ").",
        "postcon("++ intercalate "," ("R" : map (varVar' oPre) oVars) ++") :- "
        ++ "postcon_run(R), "
        ++ concat ["int("++ varVar' oPre v ++"), " | v <- oVars]
        ++ concat ["int("++ varVar v ++"), " | v <- plVars]
        ++ concat ["in(R,"++ varTerm v ++","++ varVar' iPre v ++"), " | v <- iVars]
        ++ postCondString ++".",
        ":- postcon_run(R), "
        ++ concat ["run_var_out(R,"++ varTerm v ++","++ varVar' oPre v ++"), "
                  | v <- oVars]
        ++ "not postcon("++ intercalate "," ("R" : map (varVar' oPre) oVars) ++")."]

      CondASPProp props -> do
          PostConditionExample{ exID=runID, exArrays=arraySizes } <- exs
          let bindings = aspBindings conf arraySizes
          let definedLits = [
               "run_var_out_set" [fromString runID, vTerm]
               | (var,(Out,_,vTerm)) <- bindings, var `isFreeIn` postCond]
          let domain var = ASP.Body $ case lookup var bindings of {
              Just (In,  _, vTerm) ->
                ["in" [fromString runID, vTerm, ASP.TVar var]];
              Just (Out, _, vTerm) ->
                ["run_var_out" [fromString runID, vTerm, ASP.TVar var],
                 ASP.CLiteral . ASP.LCompare $
                    ASP.CBiOp ASP.CNE (ASP.ETerm $ ASP.TVar var) "unset"];
              Nothing -> [] }
          let prop = fromMaybe (error $ show arraySizes ++" not in "++ show props) $
                     lookup arraySizes props
          let prop' = foldr (Logic.PAnd . Logic.PAtom) prop definedLits
          map show $ ASP.propToRules domain (ASP.Head []) (negation prop')

      CondEmpty -> []

    allVars   = nub . sort $ inputVars ++ outputVars ++ extraVars
    allArrays = map fst arrays
    
    Conf{ cfTimeMax        =timeMax,   cfIntRange      =(intMin, intMax),
          cfInputVars      =inputVars, cfOutputVars    =outputVars,
          cfExtraVars      =extraVars, cfConstants     =constants,
          cfDisallow       =disallow,  cfReadOnly      =readOnly,
          cfIfStatementsMax=ifMax,     cfWhileLoopsMax =whileMax,
          cfLogicVars      =logicVars, cfPostCondition =postCond,
          cfArrays         =arrays,    cfReadOnlyArrays=roArrays,
          cfStackMax       =stackMax } = conf

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
                guard $ or [True | (compare aName -> EQ, _) <- arrays]
                return (aName, aLen)
            inputs <- return $ do
                Fact (Name "counter_in") args <- answer
                let [tName, TInt value] = args
                let atName = termToASP tName
                let name = fromJust $ readVarTerm tName
                guard $ or [True | (_, (In, _, compare atName -> EQ)) <- bindings]
                return (name, value)
            outputs <- return . listToMaybe $ do
                Fact "counter_halt" [] <- answer
                return . Output $ do
                    Fact (Name "counter_out") args <- answer
                    [tName, TInt value] <- [args]
                    let atName = termToASP tName
                    let name = fromJust $ readVarTerm tName
                    guard $ or [True | (_, (Out, _, compare atName -> EQ)) <- bindings]
                    return (name, value)
            expected <- return $ do
                Fact "any_expected" [] <- answer
                return . Output $ do
                    Fact (Name "expect") args <- answer
                    [tName, TInt value] <- [args]
                    let name = fromJust $ readVarTerm tName
                    return (name, value)
            return $ Just $ Counterexample{
                ceArrays         = M.fromList $ eArrays,
                ceInput          = Input inputs,
                ceActualOutput   = outputs,
                ceExpectedOutput = expected }
        _ -> return Nothing
  where
    bindings = aspBindings conf (M.fromList arrays)
    Conf{ cfArrays=arrays } = conf

findCounterexampleASP :: Program -> Conf -> String
findCounterexampleASP prog conf
  = unlines . intercalate [""] $ [
        headerLines, programLines, preCondLines, postCondLines]
  where
    headerLines = [
        "#const time_max=" ++ show timeMax ++ ".",
        "#const int_min=" ++ show intMin ++ ".",
        "#const int_max=" ++ show intMax ++ ".",
        "#const stack_max=" ++ show stackMax ++ ".",
        "#const line_max=0.",
        "#include \"counterexample.lp\"."] ++
        (guard (not . null $ inputVars) >>
         ["input_var("  ++ intercalate "; " (map varTerm inputVars)  ++ ")."]) ++
        (guard (not . null $ outputVars) >>
         ["output_var(" ++ intercalate "; " (map varTerm outputVars) ++ ")."]) ++
        (guard (not . null $ inputArrays) >>
         ["input_array("  ++ intercalate ";" inputArrays ++ ")."]) ++
        (guard (not . null $ outputArrays) >>
         ["output_array(" ++ intercalate ";" outputArrays ++ ")."])

    programLines =
        let Program instrs = prog in Clingo.showFactLines instrs

    preCondLines = case preCond of
      CondASPString preCondStr ->
        let preVars = filter ((`isFreeIn` preCond) . varVar' iPre) inputVars in
        let inDom   = ["counter_in("++ varTerm v ++", "++ varVar' iPre v ++")" | v<-preVars] in [
        let preConds = filter (not . null) [preCondStr] in
        "precon :- "++ intercalate ", " (preConds ++ inDom) ++".",
        ":- not precon."]
      CondASPProp props -> do
          (arraySizes, prop) <- props
          let bindings = aspBindings conf (M.fromList $ cfArrays conf)
          let domain var = ASP.Body $ case lookup var bindings of {
            Just (In, _, vTerm) -> ["counter_in" [vTerm, ASP.TVar var]];
            _                   -> [] }
          let arraySizeLits =
               ["counter_array" [fromString a, ASP.TInt s] | (a,s) <- M.toList arraySizes]
          let definedLits =
               ["counter_in_any" [vTerm] | (_,(In,_,vTerm)) <- aspBindings conf arraySizes]
          let prop'  = foldr (Logic.PAnd . Logic.PAtom) prop definedLits
          let prop'' = foldr (Logic.PAnd . Logic.PAtom) (negation prop') arraySizeLits
          map show $ ASP.propToRules domain (ASP.Head []) prop''
      CondEmpty -> []
    
    postCondLines = case postCond of
      CondASPString postCondStr ->
        let inVars = filter ((`isFreeIn` postCond) . varVar' iPre) inputVars in
        let inDom = ["counter_in("++ varTerm v ++", "++ varVar' iPre v ++")" | v<-inVars] in
        let plVars = [v | v <- (logicVars \\ outputVars),
                          varVar v `isFreeIn` postCond] in
        let postOutDom = ["int("++ varVar' iPre v ++")" | v <- outputVars]
                      ++ ["int("++ varTerm v ++")" | v <- plVars] in
        let postConds = filter (not . null) [postCondStr] in
        let ruleBody = intercalate ", " (postConds ++ inDom ++ postOutDom) in
        let outVars = filter ((`isFreeIn` postCond) . varVar' oPre) outputVars in
        let args = case outVars of {
            [] -> "";
            _  -> "(" ++ intercalate ", " (map (varVar' oPre) outVars) ++ ")" } in
        let actOutDom = [
             "counter_out("++ varTerm v ++", "++ varVar' oPre v ++")"
             | v <- outVars] in
        let expOutDom = [
             "expect("++ varTerm v ++","++ varVar' oPre v ++")"
             | v <- outVars] in
        let consHead = "postcon" ++ args in [
        "postcon"++ args ++" :- "++ ruleBody ++".",
        ":- " ++ intercalate ", " (consHead : actOutDom) ++ ".",
        ":- " ++ intercalate ", " (("not "++consHead) : expOutDom) ++ "."]
      CondASPProp props -> do
          (arraySizes, prop) <- props
          let localBindings = aspBindings conf arraySizes

          let oDomain var = ASP.Body $ case lookup var localBindings of {
            Just (In,  _, vTerm) -> ["counter_in"  [vTerm, ASP.TVar var]];
            Just (Out, _, vTerm) -> [
                "counter_out" [vTerm, ASP.TVar var],
                ASP.CLiteral . ASP.LCompare $
                    ASP.CBiOp ASP.CNE (ASP.ETerm $ ASP.TVar var) "unset"];
            Nothing              -> [] }
          let eDomain var = ASP.Body $ case lookup var localBindings of {
            Just (In,  _, vTerm) -> ["counter_in"  [vTerm, ASP.TVar var]];
            Just (Out, _, vTerm) -> ["expect" [vTerm, ASP.TVar var]];
            Nothing              -> [] }

          let arraySizeLits = 
               ["counter_array" [fromString a, ASP.TInt s]
               | (a,s) <- M.toList arraySizes]
          let inDefinedLits =
               ["counter_in_any" [vTerm]
               | (_,(In,_,vTerm)) <- localBindings]
          let outDefinedLits =
               ["any_actual_var" [vTerm]
               | (var,(Out,_,vTerm)) <- localBindings, var `isFreeIn` postCond]
 
          let outProp = foldr (Logic.PAnd . Logic.PAtom) prop $
                        inDefinedLits ++ outDefinedLits ++ arraySizeLits

          let expProp = foldr (Logic.PAnd . Logic.PAtom) prop $
                        inDefinedLits
          let expProp' = foldr (Logic.PAnd . Logic.PAtom) (negation expProp)
                         arraySizeLits

          (map show $
              ASP.propToRules oDomain mempty outProp ++
              ASP.propToRules eDomain mempty expProp')
      CondEmpty -> [":-."]

    Conf{ cfIntRange=(intMin, intMax), cfTimeMax=timeMax,
          cfInputVars=inputVars, cfOutputVars=outputVars,
          cfInputArrays=inputArrays, cfOutputArrays=outputArrays,
          cfPreCondition=preCond, cfPostCondition=postCond,
          cfLogicVars=logicVars, cfStackMax=stackMax } = conf

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
-- Miscellaneous utilities

runClingoConf :: [ClingoInput] -> Maybe String -> Conf -> IO ClingoResult
runClingoConf input maybeRunID conf = do
    let options = runClingoOptions{
        rcEchoStdout = cfEchoClingo conf,
        rcEchoInput  = cfEchoASP conf,
        rcIdentifier = maybeRunID,
        rcEcho       = cfPutLines conf }
    runClingo options (input ++ [CIFile $ cfConfFile conf])

maybePrintTime :: Conf -> IO a -> IO a
maybePrintTime conf mx = case cfPrintTime conf of
    True -> do
        (x, elapsed) <- time mx
        cfPutLines conf [ansiDarkYellow ++"("++ show elapsed ++")"++ ansiClear]
        return x
    False -> mx

--------------------------------------------------------------------------------
showProgram :: Program -> [String]
showProgram (Program [])
  = ["   (empty program)"]
showProgram (Program facts)
  = While.showProgram . catMaybes . map While.readLineInstr $ facts

programLength :: Program -> Integer
programLength (Program facts)
  = genericLength [() | Just (("main",_),_) <- map While.readLineInstr facts]

showCounterexample :: Counterexample -> [String]
showCounterexample cex
  = showInput ++ showExpected ++ showActual
  where
    showInput
      = ["    Input:    " ++ showVars (unInput input)]
    showExpected
      = ["    Expected: " ++ case expected of
            []            -> "(unsatisfiable)"
            Output es : _ -> showVars es ++" | ..."]
    showActual
      = ["    Output:   " ++ case actual of {
            Nothing          -> "(does not halt)";
            Just (Output os) -> showVars os}]

    showVars :: [(WhileVar, Value)] -> String
    showVars vars
      | null parts = "(empty)"
      | otherwise  = intercalate ", " parts
      where
        parts = [a ++" = "++ s | (a,s) <- arrayStrings]
             ++ [show v ++" = "++ show c | (v,c) <- sort vars, showVar v]
        arrayStrings = do
            (aName, aSize) <- M.toList arrays
            cs <- maybeToList $ sequence [
                lookup (VArray aName i) vars | i <- [0..aSize-1]]
            return (aName, "["++ intercalate ", " (map show cs) ++"]")

        showVar (VArray a _) = not $ a `elem` map fst arrayStrings
        showVar _            = True

    Counterexample{
        ceArrays         = arrays,
        ceInput          = input,
        ceActualOutput   = actual,
        ceExpectedOutput = expected } = cex

--------------------------------------------------------------------------------
readVarTerm :: Term -> Maybe WhileVar
readVarTerm term = case term of
    TFun (Name v) []                             -> Just $ VScalar v
    TFun (Name "array") [TFun(Name a)[], TInt i] -> Just $ VArray  a i
    _                                            -> Nothing

varTerm :: WhileVar -> String
varTerm = show . varTerm'

varTerm' :: WhileVar -> ASP.Term
varTerm' (VScalar v)  = fromString (headLow v)
varTerm' (VArray a i) = "array" [fromString (headLow a), ASP.TInt i]

varVar :: WhileVar -> String
varVar = varVar' ""

varVar' :: String -> WhileVar -> String
varVar' p (VScalar v)  = headUp $ p ++ v
varVar' p (VArray a i) = headUp $ p ++ a ++"_"++ show i

instance Show WhileVar where
    show (VScalar v)  = v
    show (VArray a i) = a ++"["++ show i ++"]"

--------------------------------------------------------------------------------
class IsFreeIn var where
    isFreeIn :: var -> Condition -> Bool

instance IsFreeIn Variable where
    isFreeIn var cond = case cond of
        CondASPString cond' -> var `ASPString.isFreeIn` cond'
        CondASPProp   props -> ASP.Variable var `oelem` ASP.FreeVars props
        CondEmpty           -> False

instance IsFreeIn ASP.Variable where
    isFreeIn var cond = case cond of
        CondASPString cond' -> show var `ASPString.isFreeIn` cond'
        CondASPProp   props -> var `oelem` ASP.FreeVars props
        CondEmpty           -> False

--------------------------------------------------------------------------------
readCondTerm :: Term -> Conf -> HaskellT IO (Maybe Condition)
readCondTerm term conf = case term of
    TStr aspCond ->
        return . Just . CondASPString $ aspCond
    TFun "asp" [TStr aspCond] ->
        return . Just . CondASPString $ aspCond
    TFun "haskell" [TStr hsExpr] -> do
        aspProp <- readHaskellCond hsExpr (hsBindings conf) (cfArrays conf)
        return . Just . CondASPProp $ aspProp
    _ -> return Nothing

readHaskellCond
  :: HsExpr
  -> (ArraySizes -> HsBindings)
  -> [(ArrayName, ArraySize)]
  -> HaskellT IO [(ArraySizes, ASPProp)]
readHaskellCond hsExpr sizesToBindings maxArraySizes = do
    eProps <- try . sequence $ do
        sizes <- M.fromList <$>
            sequence [[(a,s) | s <- [0..m]] | (a,m) <- maxArraySizes]
        let bindings = sizesToBindings sizes
        return $ do
            bool <- haskellToBoolT hsExpr bindings
            return (sizes, boolToPropASP bool)
    case eProps of
        Left (HaskellError err) -> liftIO $ do
            putStrLn $ "Error in Haskell condition \""++ hsExpr ++"\":"
            putStrLn err
            exitFailure
        Right props -> return props

hsBindings :: Conf -> ArraySizes -> HsBindings
hsBindings conf arraySizes
  = (varBind   "in_" <$> inVars)   ++ (varBind   "out_" <$> outVars)   ++
    (arrayBind "in_" <$> inArrays) ++ (arrayBind "out_" <$> outArrays) ++
    (varBind    ""   <$> logicVars)
  where
    varBind :: String -> WhileVar -> (HsVar, HsInput)
    varBind prefix var = (
        headLow $ prefix ++ varTerm var,
        HsScalar . Abstract.IVar $ varVar' prefix var)

    arrayBind :: String -> ArrayName -> (HsVar, HsInput)
    arrayBind prefix name = (
        headLow $ prefix ++ name,
        HsArray [Abstract.IVar . headUp $ varVar' prefix (VArray name i)
            | s <- maybeToList $ M.lookup name arraySizes, i <- [0..s-1]])

    Conf{ cfInputVars   = inVars,    cfOutputVars   = outVars,
          cfInputArrays = inArrays,  cfOutputArrays = outArrays,
          cfLogicVars   = logicVars} = conf

-- The correspondence between ASP variables and terms representing program variables.
aspBindings :: Conf -> ArraySizes -> [(ASP.Variable, (Direction, WhileVar, ASP.Term))]
aspBindings conf arraySizes
  = [(fromString $ varVar' iPre v, (In,  v, varTerm' v)) | v <- allInVars] 
  ++[(fromString $ varVar' oPre v, (Out, v, varTerm' v)) | v <- allOutVars]
  ++[(fromString $ varVar v, (In, v, varTerm' v)) | v <- logicVars \\ outVars]
  where
    allInVars  = (inVars \\ logicVars) ++ concatMap arrVars inArrs
    allOutVars = outVars ++ concatMap arrVars outArrs
    arrVars a  = [VArray a i | (compare a -> EQ, len) <- M.toList arraySizes,
                               i <- [0..len-1]]
    Conf{ cfInputVars=inVars, cfOutputVars=outVars, cfLogicVars=logicVars,
          cfInputArrays=inArrs, cfOutputArrays=outArrs } = conf
