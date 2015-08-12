{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}

module Clingo where

import System.IO
import System.Process
import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.List
import Data.Char
import Data.String

import qualified ASP
import Util

type Parse a = String -> Maybe (a, String)

newtype Name = Name String deriving (Eq, Show, IsString)
data Fact = Fact Name [Term] deriving Show
data Term = TInt Integer | TStr String | TFun Name [Term] deriving Show

-- ASP code passed to runClingo.
data ClingoInput
   = CICode String
   | CIFile FilePath
   deriving Show

-- Return value of runClingo.
data ClingoResult
   = CRSatisfiable [Answer]
   | CRUnsatisfiable
   deriving Show

type Answer = [Fact]

-- Options passed to runClingo.
data RunClingoOptions = RunClingoOptions{
    -- Additional arguments to be passed to Clingo.
    rcClingoArgs :: [String],      

    -- If True, Clingo's stdout is copied to the current stdout.
    rcEchoStdout :: Bool,          

    -- If True, the input ASP is copied to current stdout.
    rcEchoInput  :: Bool,

    -- The function to use to write blocks of console lines.
    rcEcho :: [String] -> IO (),

    -- If True, a textual prefix is included to distinguish input from output.
    rcEchoPrefix :: Bool,
    
    -- If True, ANSI colour codes are included to distinguish input from output.
    rcEchoColour :: Bool,
    
    -- If given, annotates the output of rcEchoStdout and rcEchoInput.
    rcIdentifier :: Maybe String }

-- RunClingoOptions with default values.
runClingoOptions :: RunClingoOptions
runClingoOptions = RunClingoOptions{
    rcClingoArgs = [],
    rcEchoStdout = True,
    rcEchoInput  = False,
    rcEcho       = mapM_ putStrLn,
    rcEchoPrefix = True,
    rcEchoColour = True,
    rcIdentifier = Nothing }

--------------------------------------------------------------------------------
-- Interoperation with ASP module.

termToASP :: Term -> ASP.Term
termToASP term = case term of
    TInt i           -> ASP.TInt i
    TStr s           -> ASP.TStr s
    TFun (Name f) ts -> ASP.TFun (ASP.Function f) (map (ASP.ETerm . termToASP) ts)

--------------------------------------------------------------------------------
-- Run Clingo 3, which is assumed to be present on the search path as 'clingo',
-- with the given input strings/files, and with the given arguments appended.
-- Returns either CRSatisfiable [answer1, answer2, ...], where the answerN are
-- given in reverse order, or CRUnsatisfiable. In particular, if an optimum
-- is reported by Clingo, this is given as the first answer in the list.
runClingo :: RunClingoOptions -> [ClingoInput] -> IO ClingoResult
runClingo options inputs = do
    let args = "-" : extraArgs
    let spec = (proc "clingo" args) { std_in=CreatePipe, std_out=CreatePipe }
    (Just clingoIn, Just clingoOut, _, clingoProc) <- createProcess spec

    codeLines <- return . concat $ do
        input <- inputs
        return $ case input of
            CICode code -> lines code
            CIFile path -> ["#include \"" ++ path ++ "\"."]
    hPutStr clingoIn (unlines codeLines)
    hClose clingoIn
    
    when echoInput . echo $ "" : do
        line <- codeLines
        prefix <- return $ case echoPrefix of
            True  -> maybe "<-- " (++ "<- ") identifier
            False -> ""
        return $ case echoColour of
            True  -> ansiDarkGreen ++ prefix ++ line ++ ansiClear
            False -> prefix ++ line    

    result <- readClingo [] clingoOut
    _ <- waitForProcess clingoProc
    return result
  where
    RunClingoOptions{
        rcClingoArgs = extraArgs,
        rcEchoStdout = echoStdout, rcEchoInput  = echoInput,
        rcEchoColour = echoColour, rcEchoPrefix = echoPrefix,
        rcEcho       = echo,       rcIdentifier = identifier } = options
    readClingo :: [[Fact]] -> Handle -> IO ClingoResult
    readClingo answers clingoOut = do
        line <- ehGetLine clingoOut
        case line of
            _ | "Answer: " `isPrefixOf` line -> do
                oLine <- ehGetLine clingoOut
                let Just (answer, "") = readFacts oLine
                readClingo (answer : answers) clingoOut
            "SATISFIABLE" ->
                return (CRSatisfiable answers)
            "OPTIMUM FOUND" ->
                return (CRSatisfiable answers)
            "UNSATISFIABLE" ->
                return CRUnsatisfiable
            _ ->
                readClingo answers clingoOut
    ehGetLine :: Handle -> IO String
    ehGetLine handle = do
        line <- hGetLine handle
        prefix <- return $ case rcEchoPrefix options of
            True  -> maybe "--> " (++ "-> ") (rcIdentifier options)
            False -> ""
        line' <- return $ case rcEchoColour options of
            True  -> ansiDarkRed ++ prefix ++ line ++ ansiClear
            False -> prefix ++ line
        when echoStdout $ rcEcho options [line']
        return line

--------------------------------------------------------------------------------
-- Read a space-separated list of facts.
readFacts :: Parse [Fact]
readFacts "" = do
    return ([], "")
readFacts str = do
    (fact, _str) <- readFact str
    (_,_str) <- return (span isSpace _str)
    (facts, _str) <- readFacts _str <|> return ([], _str)
    return (fact:facts, _str)

showFacts :: [Fact] -> String
showFacts facts = concat (intersperse " " (map showFact facts))

showFactLines :: [Fact] -> [String]
showFactLines facts = map ((++ ".") . showFact) facts

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
  = readValue TInt str  <|> readValue TStr str <|>
    readSymbol TFun str <|> readTuple str 

showTerm :: Term -> String
showTerm term = case term of
    TInt int       -> show int
    TStr str       -> show str
    TFun name args -> showSymbol name args

--------------------------------------------------------------------------------
readSymbol :: (Name -> [Term] -> a) -> Parse a
readSymbol con str = do
    (name, _str) <- readName str
    (args, _str) <- readArgs _str <|> return ([], _str)
    return (con name args, _str)

showSymbol :: Name -> [Term] -> String
showSymbol (Name name) args = name ++ showArgs args

--------------------------------------------------------------------------------
readValue :: Read a => (a -> b) -> Parse b
readValue con str = do
    (value, _str) <- listToMaybe (reads str)
    return (con value, _str)

--------------------------------------------------------------------------------
readName :: Parse Name
readName str = do
    guard (any isLower $ take 1 str)
    let (name, rest) = span (\c -> isAlphaNum c || c == '_') str
    return (Name name, rest)

--------------------------------------------------------------------------------
readTuple :: Parse Term
readTuple str = do
    (args, _str) <- readArgs str
    return $ (TFun "" args, _str)

--------------------------------------------------------------------------------
readArgs :: Parse [Term]
readArgs str = do
    '(':_str <- return str
    (_,_str) <- return (span isSpace _str)
    (arg, _str) <- readTerm _str
    (args, _str) <- readArgs' _str <|> return ([], _str)
    (_,_str) <- return (span isSpace _str)
    ')':_str <- return _str
    return (arg:args, _str)

readArgs' :: Parse [Term]
readArgs' str = do
    (_,_str) <- return (span isSpace str)
    ',':_str <- return _str
    (_,_str) <- return (span isSpace _str)
    (arg, _str) <- readTerm _str
    (args, _str) <- readArgs' _str <|> return ([], _str)
    return (arg:args, _str)

showArgs :: [Term] -> String
showArgs []   = ""
showArgs args = "(" ++ concat (intersperse "," (map showTerm args)) ++ ")"
