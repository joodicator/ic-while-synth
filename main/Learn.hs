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
    cfClingoArgs :: [String],
    cfParamFile  :: FilePath }

data State = State{
    stLineCount :: Int,
    stConf      :: Conf }

main = do
    hSetBuffering stdout NoBuffering
    conf <- getConf
    learn State{
        stLineCount = 0,
        stConf      = conf }

getConf :: IO Conf
getConf = getArgs >>= \args -> case args of
    paramFile : clingoArgs -> do
        return Conf{ cfParamFile=paramFile, cfClingoArgs=clingoArgs }
    _ -> do
        progName <- getProgName
        hPutStrLn stderr $ "Usage: " ++ progName ++ " PARAM_FILE CLINGO_ARGS..."
        exitFailure

learn :: State -> IO ()
learn state = do
    putStrLn $ "\ESC[1m=== line_max=" ++ show (stLineCount state) ++ " ===\ESC[0m"
    let options = runClingoOptions{
        rcEchoPrefix = False,
        rcEchoColour = False,
        rcClingoArgs = cfClingoArgs (stConf state) }
    result <- runClingo options [
        CICode ("#const line_max=" ++ show (stLineCount state) ++ "."),
        CIFile (cfParamFile $ stConf state),
        CIFile "learn.lp"]
    putStrLn ""
    case result of
        CRSatisfiable _ -> exitSuccess
        CRUnsatisfiable -> learn state{ stLineCount = stLineCount state + 1 }
