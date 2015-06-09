module Clingo where

import System.IO
import System.Process
import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.List
import Data.Char

type Parse a = String -> Maybe (a, String)
newtype Name = Name String deriving (Eq, Show)
data Fact = Fact Name [Term] deriving Show
data Term = TInt Integer | TStr String | TFun Name [Term] deriving Show

type ClingoArg = String
type Answer = [Fact]

data ClingoInput
   = CICode String | CIFile FilePath
   deriving Show

data ClingoResult
   = Satisfiable [Answer] | Unsatisfiable
   deriving Show

--------------------------------------------------------------------------------
runClingo :: [ClingoArg] -> [ClingoInput] -> IO ClingoResult
runClingo postArgs inputs = do
    let args = "-" : postArgs
    let spec = (proc "clingo" args) { std_in=CreatePipe, std_out=CreatePipe }
    (Just clingoIn, Just clingoOut, _, clingoProc) <- createProcess spec
    forM inputs $ \input -> case input of
        CICode code -> hPutStr clingoIn code
        CIFile path -> hPutStr clingoIn =<< readFile path
    hClose clingoIn
    result <- readClingo [] clingoOut
    waitForProcess clingoProc
    return result
  where
    readClingo :: [[Fact]] -> Handle -> IO ClingoResult
    readClingo rAnswers clingoOut = do
        line <- ehGetLine clingoOut
        case line of
            _ | "Answer: " `isPrefixOf` line -> do
                line <- ehGetLine clingoOut
                let Just (answer, "") = readFacts line
                readClingo (answer : rAnswers) clingoOut
            "SATISFIABLE" ->
                return (Satisfiable $ reverse rAnswers)
            "UNSATISFIABLE" ->
                return Unsatisfiable
            _ ->
                readClingo rAnswers clingoOut
    ehGetLine :: Handle -> IO String
    ehGetLine handle = do
        line <- hGetLine handle
        putStrLn line
        return line

--------------------------------------------------------------------------------
-- Read a space-separated list of facts.
readFacts :: Parse [Fact]
readFacts str = do
    (fact, str) <- readFact str
    (_,str) <- return (span isSpace str)
    (facts, str) <- readFacts str <|> return ([], str)
    return (fact:facts, str)

showFacts :: [Fact] -> String
showFacts facts = concat (intersperse " " (map showFact facts))

--------------------------------------------------------------------------------
-- Read a fact: a predicate symbol with 0 or more term arguments.
readFact :: Parse Fact
readFact = readSymbol Fact

showFact :: Fact -> String
showFact (Fact name args) = showSymbol name args

--------------------------------------------------------------------------------
-- Read a term: an integer, string, name, or function symbol with arguments.
readTerm :: Parse Term
readTerm str
  = readValue TInt str <|> readValue TStr str <|> readSymbol TFun str

showTerm term = case term of
    TInt int       -> show int
    TStr str       -> show str
    TFun name args -> showSymbol name args

--------------------------------------------------------------------------------
readSymbol :: (Name -> [Term] -> a) -> Parse a
readSymbol con str = do
    (name, str) <- readName str
    (args, str) <- readArgs str <|> return ([], str)
    return (con name args, str)

showSymbol :: Name -> [Term] -> String
showSymbol (Name name) args = name ++ showArgs args

--------------------------------------------------------------------------------
readValue :: Read a => (a -> b) -> Parse b
readValue con str = do
    (value, str) <- listToMaybe (reads str)
    return (con value, str)

--------------------------------------------------------------------------------
readName :: Parse Name
readName str = do
    guard (any isLower $ take 1 str)
    let (name, rest) = span (\c -> isAlphaNum c || c == '_') str
    return (Name name, rest)

--------------------------------------------------------------------------------
readArgs :: Parse [Term]
readArgs str = do
    '(':str <- return str
    (arg, str) <- readTerm str
    (args, str) <- readArgs' str <|> return ([], str)
    ')':str <- return str
    return (arg:args, str)

readArgs' :: Parse [Term]
readArgs' str = do
    ',':str <- return str
    (arg, str) <- readTerm str
    (args, str) <- readArgs' str <|> return ([], str)
    return (arg:args, str)

showArgs :: [Term] -> String
showArgs []   = ""
showArgs args = "(" ++ concat (intersperse "," (map showTerm args)) ++ ")"
