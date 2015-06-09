#!/usr/bin/env runhaskell

import System.IO
import System.Environment
import System.Exit
import System.Process

import Control.Applicative
import Data.Char
import Data.List hiding (dropWhileEnd)

import Clingo

data Conf = Conf{
    clingoArgs :: [String],
    paramFile  :: FilePath }

data State = State{
    lineCount      :: Int,
    conf           :: Conf }

main = do
    hSetBuffering stdout NoBuffering
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
    result <- runClingo (clingoArgs $ conf state) [
        CICode ("#const line_max=" ++ show (lineCount state) ++ "."),
        CIFile (paramFile $ conf state),
        CIFile "learn.lp"]
    putStrLn ""
    case result of
        Satisfiable _ -> exitSuccess
        Unsatisfiable -> learn state{ lineCount = lineCount state + 1 }
