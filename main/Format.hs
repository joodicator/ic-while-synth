#!/usr/bin/env runhaskell

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.List

import Clingo
import While

main = do
    linesIn <- lines <$> getContents
    let linesOut = concatMap convertLine linesIn
    mapM_ putStrLn linesOut

convertLine :: String -> [String]
convertLine line = fromMaybe [line] $ do
    (facts, "") <- readFacts line
    guard (not . null $ facts)
    let eLineInstrs = [maybe (Left f) Right (readLineInstr f) | f <- facts]
    let (ignoredFacts, lineInstrs) = partitionEithers eLineInstrs
    let suffix = if null ignoredFacts then [] else [showFacts ignoredFacts]
    return (showProgram lineInstrs ++ suffix)

partitionEithers
  = foldr f ([],[])
  where f (Left  l) (ls,rs) = (l:ls,rs)
        f (Right r) (ls,rs) = (ls,r:rs)
