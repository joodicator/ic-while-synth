#!/usr/bin/env runhaskell

{-# LANGUAGE ViewPatterns, BangPatterns, FlexibleContexts,
             OverloadedStrings #-}

module Main where

import qualified Data.Attoparsec.ByteString.Char8 as A

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)

import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)

import Data.List
import Control.Monad
import Control.Applicative
import System.IO

type Count = Int
type Pred  = ByteString
type Arity = Int
type HeadK = [(Pred, Arity)]
type BodyK = [(Pred, Arity, Bool)]

main :: IO ()
main = main' M.empty

main' :: Map (Maybe HeadK) (Count, Map BodyK Count) -> IO ()
main' !counts = do
    !eof <- isEOF
    case eof of
      False -> do
        !line <- B.getLine
        case A.parseOnly rule line of
            !(Right !(!hKey, !bKey)) -> do
                let !(!count, !bMap) = M.findWithDefault (0, M.empty) (Just hKey) counts
                main' $! M.insert (Just hKey) (count+1, M.insertWith (+) bKey 1 bMap) counts
            !(Left _) -> do
                let !(!count, !bMap) = M.findWithDefault (0, M.empty) Nothing counts
                main' $! M.insert Nothing (count+1, bMap) counts
      True ->
        main'' $! counts

main'' :: Map (Maybe HeadK) (Count, Map BodyK Count) -> IO ()
main'' !counts = do
    let sortedHeads = sortBy (compare `on` (fst . snd)) (M.toList counts)
    forM_ sortedHeads $ \(mhKey, (count, bMap)) -> do
        B.putStr $ B.pack (show count) +++ "\t" +++ case mhKey of
            Nothing   -> "unknown"
            Just hKey -> showHKey hKey
        case M.toList bMap of
            [([], _)] -> do
                B.putStrLn "."
            [(bKey@(_:_), _)] -> do
                B.putStrLn $ " " +++ showBKey bKey
            _ -> do
                B.putStrLn ""
                let sortedBodies = sortBy (compare `on` snd) $ M.toList bMap
                forM_ sortedBodies $ \(bKey, bCount) -> do
                    B.putStrLn $ "\t" +++ B.pack (show bCount) +++ "\t" +++ showBKey bKey
  where
    on f g x y  = f (g x) (g y)

--------------------------------------------------------------------------------
-- Parsing of ground rules:

rule :: A.Parser (HeadK, BodyK)
rule = (,) <$> headK <*> (A.string ":-" *> bodyK1 <|> pure []) <* A.char '.'

headK :: A.Parser HeadK
headK = headK1 <|> pure []

headK1 :: A.Parser HeadK
headK1 = (:) <$> predArity <*> (A.char '|' *> headK1 <|> pure [])

bodyK1 :: A.Parser BodyK
bodyK1 = (:) <$> atom <*> (A.char ',' *> bodyK1 <|> pure [])

atom :: A.Parser (Pred, Arity, Bool)
atom = do void (A.string "not "); (p,a) <- predArity; return (p,a,False)
   <|> do                         (p,a) <- predArity; return (p,a,True)

predArity :: A.Parser (Pred, Arity)
predArity = (,) <$> predName <*> (A.char '(' *> args1 <* A.char ')' <|> pure 0)

args1 :: A.Parser Arity
args1 = (+ 1) <$> (term *> (A.char ',' *> args1 <|> pure 0))

predName :: A.Parser Pred
predName
  = B.cons <$> A.letter_ascii <*> A.takeWhile isPred
  where
    isPred c = A.isAlpha_ascii c || A.isDigit c || c == '_'

term :: A.Parser ()
term = ter where
    ter  = A.skipMany $ par <|> str <|> A.satisfy (A.notInClass ",)")
    ter' = A.skipMany $ par <|> str <|> A.notChar ')'
    par  = A.char '(' *> ter' *> A.char ')'
    str  = A.char '"' *> str'
    str' = A.char '"' <|> (esc <|> A.anyChar) *> str'
    esc  = A.char '\\' *> A.anyChar

--------------------------------------------------------------------------------
-- Miscellaneous utilities:

showHKey :: HeadK -> ByteString
showHKey [] = "/0"
showHKey ps = B.intercalate " | " [
    p +++ "/" +++ B.pack (show a) | (p,a) <- ps]

showBKey :: BodyK -> ByteString
showBKey ps = ":- " +++ B.intercalate ", " [
    (if b then "" else "not ") +++ p +++ "/" +++ B.pack (show a)
    | (p,a,b) <- ps] +++ "."

infixr 5 +++
(+++) :: ByteString -> ByteString -> ByteString
(+++) = B.append
