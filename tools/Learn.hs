#!/usr/bin/env runhaskell

import System.IO
import System.Environment
import System.Exit
import System.Process

import Control.Applicative
import Data.Char
import Data.List hiding (dropWhileEnd)

data Conf = Conf{
    clingoArgs :: [String],
    paramFile  :: FilePath }

data State = State{
    lineCount      :: Int,
    conf           :: Conf }

data ClingoResult
  = Satisfiable | Unsatisfiable

main = do
    conf <- getConf
    learn State{
        lineCount      = 1,
        conf           = conf }

getConf :: IO Conf
getConf = getArgs >>= \args -> case args of
    paramFile : clingoArgs -> do
        return Conf{ paramFile=paramFile, clingoArgs=clingoArgs }
    _ -> do
        progName <- getProgName
        hPutStrLn stderr $ "Usage: " ++ progName ++ " PARAM_FILE CLINGO_ARGS..."
        exitFailure

learn :: State -> IO ()
learn state = do
    putStrLn $ "\ESC[1m=== line_max=" ++ show (lineCount state) ++ " ===\ESC[0m"
    let args = ["-", paramFile (conf state), "learn.lp"]
    let spec = (proc "clingo" args) { std_in=CreatePipe, std_out=CreatePipe }
    (Just clingoIn, Just clingoOut, _, clingoProc) <- createProcess spec
    hPutStrLn clingoIn $ "#const line_max=" ++ show (lineCount state) ++ "."
    hClose clingoIn
    result <- readClingo clingoOut
    waitForProcess clingoProc
    putStrLn $ ""
    case result of
        Just Satisfiable   -> exitSuccess
        Just Unsatisfiable -> learn state{ lineCount = lineCount state + 1 }
        Nothing            -> exitFailure

readClingo :: Handle -> IO (Maybe ClingoResult)
readClingo clingoOut = do
    readable <- hIsReadable clingoOut
    isEOF <- hIsEOF clingoOut
    case readable && not isEOF of
        True -> do
            line <- hGetLine clingoOut
            putStrLn line
            let result = case line of {
                "SATISFIABLE"   -> Just Satisfiable;
                "UNSATISFIABLE" -> Just Unsatisfiable;
                _               -> Nothing }
            laterResult <- readClingo clingoOut
            return $ laterResult <|> result
        False -> do
            return Nothing
