import Data.List

import qualified While
import qualified Clingo

type Variable  = String
type Value     = String
type Condition = String

newtype Program  = Program [While.Instr]
newtype Input    = Input   [(Variable, Value)]
newtype Output   = Output  [(Variable, Value)]

data Conf = Conf {
    cfTimeMax    :: Integer,
    cfIntMax     :: Integer,
    cfInputVars  :: [Variable],
    cfOutputVars :: [Variable],
    cfPreCond    :: Condition,
    cfPostCond   :: Condition}

data Counterexample = Counterexample {
    ceInput          :: Input,
    ceActualOutput   :: Output,
    ceExpectedOutput :: Output}

findCounterexample :: Conf -> Program -> IO Counterexample
findCounterexample conf prog = do
    let { header = unlines [
        "#const time_max=" ++ show timeMax ++ ".",
        "#const int_max=" ++ show intMax ++ "." ,
        "#include \"counterexample.lp\".",
        "input_var(" ++ intercalate ";" (map ("var_" ++) inputVars) ++ ").",
        "output_var(" ++ intercalate "; " (map ("var_" ++) outputVars) ++ ").",
        let params = intercalate ", " $ map ("In_" ++) preVars in
        let body = intercalate ", " $ preCond : preDomain in
        "precon("++ params ++") :- " ++ body ++ "."] }
    putStrLn header
    return undefined
  where
    preVars     = [v | v <- inputVars,  ("In_"++v)  `isInfixOf` preCond]
    postInVars  = [v | v <- inputVars,  ("In_"++v)  `isInfixOf` postCond]
    postOutVars = [v | v <- outputVars, ("Out_"++v) `isInfixOf` postCond]
    preDomain   = ["counter_in(var_"++v++", In_"++v++")"   | v <- preVars]
    postDomain  = ["counter_in(var_"++v++", In_"++v++")"   | v <- postInVars]
               ++ ["counter_out(var_"++v++", Out_"++v++")" | v <- postOutVars]
    Conf{
        cfTimeMax   = timeMax,   cfIntMax     = intMax,
        cfInputVars = inputVars, cfOutputVars = outputVars,
        cfPreCond   = preCond,   cfPostCond   = postCond } = conf
