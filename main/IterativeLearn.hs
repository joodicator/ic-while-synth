#!/usr/bin/env runhaskell

module Main where

import IterativeLearn.Base

import System.Environment
import Control.Monad
import Data.List

main :: IO ()
main = do
    args <- getArgs
    let ([confPath], conf) = readArgs args defaultConf

    when (cfPrintTime conf) $ putStrLn "Reading configuration file..."
    _conf <- maybePrintTime conf $ readConfFile confPath conf

    void $ iterativeLearnConf _conf

readArgs :: [String] -> Conf -> ([String], Conf)
readArgs args@(arg : _) conf | "-" `isPrefixOf` arg
  = readArgsFlag  args conf
readArgs (arg : args) conf | otherwise
  = let (args', conf') = readArgs args conf in (arg:args', conf')
readArgs [] conf
  = ([], conf)

readArgsFlag :: [String] -> Conf -> ([String], Conf)
readArgsFlag (arg : args) conf
  | arg `elem` ["-i", "--interactive"]
  = readArgs args (conf{ cfInteractive=True })
  | arg `elem` ["--echo-clingo", "-ec"]
  = readArgs args (conf{ cfEchoClingo=True })
  | arg `elem` ["--echo-asp", "-ea"]
  = readArgs args (conf{ cfEchoASP=True })
  | arg `elem` ["--time", "-t"]
  = readArgs args (conf{ cfPrintTime=True })
  | arg `elem` ["-j", "--threads"]
  = let param : args' = args in
    readArgs args' (conf{ cfThreads = read param })
  | "-j" `isPrefixOf` arg
  = let Just param = stripPrefix "-j" arg in
    readArgs args (conf{ cfThreads = read param })
  | "--threads=" `isPrefixOf` arg
  = let Just param = stripPrefix "--threads=" arg in
    readArgs args (conf{ cfThreads = read param })
  | otherwise
  = error $ "Unrecognised option: " ++ arg
readArgsFlag [] conf
  = ([], conf)
