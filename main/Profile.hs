#!/usr/bin/env runhaskell

{-# LANGUAGE ViewPatterns #-}

module Main where

import ASP
import qualified ASP.Parse as Parse
import Text.Parsec

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List
import Control.Monad
import System.IO
import System.Exit

main = main' M.empty

main' :: M.Map (Maybe (S.Set (String, Int))) Int -> IO ()
main' counts = do
    eof <- isEOF
    case eof of
      False -> do
        result <- runParserT Parse.rule () "" =<< getLine
        key <- case result of
            Right (Head hs :- _) -> do
                return . Just . S.fromList $ map key hs
            Left _ -> do
                return Nothing
        main' $ M.insertWith (+) key 1 counts
      True ->
        main'' counts

main'' :: M.Map (Maybe (S.Set (String, Int))) Int -> IO ()
main'' counts
  = forM_ (M.toList counts) $ \(k, v) -> do
        ks <- return $ case k of
            Nothing               -> "unknown"
            Just (S.toList -> hs) -> keys hs
        putStrLn $ show v ++ "\t" ++ ks

key :: Atom -> (String, Int)
key (Atom (Predicate name) args) = (name, length args)

keys :: [(String, Int)] -> String
keys [] = "/0"
keys ks = intercalate " | " [p ++"/"++ show n | (p,n) <- ks]
