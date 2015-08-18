#!/usr/bin/env runhaskell

{-# LANGUAGE ViewPatterns, FlexibleContexts #-}

module Main where

import ASP
import qualified ASP.Parse as Parse
import Text.Parsec

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List
import Data.Monoid
import Control.Applicative hiding ((<|>))
import Control.Monad
import System.IO
import System.Exit

parseHead :: Parse.Parse s u m Head
parseHead
  = option mempty Parse.head <* (string "." <|> string ":-") <* skipMany anyToken

main = main' M.empty

main' :: M.Map (Maybe (S.Set (String, Int))) Int -> IO ()
main' counts = do
    eof <- isEOF
    case eof of
      False -> do
        line <- getLine
        let result = parse parseHead "" line
        key <- case result of
            Right (Head hs) -> do
                return . Just . S.fromList $ map key hs
            Left _ -> do
                return Nothing
        main' $! M.insertWith (+) key 1 counts
      True ->
        main'' counts

main'' :: M.Map (Maybe (S.Set (String, Int))) Int -> IO ()
main'' counts
  = forM_ (sortBy (compare `on` snd) (M.toList counts)) $ \(k, v) -> do
        ks <- return $ case k of
            Nothing               -> "unknown"
            Just (S.toList -> hs) -> keys hs
        putStrLn $ show v ++ "\t" ++ ks
  where
    on f g x y = f (g x) (g y)

key :: Atom -> (String, Int)
key (Atom (Predicate name) args) = (name, length args)

keys :: [(String, Int)] -> String
keys [] = "/0"
keys ks = intercalate " | " [p ++"/"++ show n | (p,n) <- ks]
