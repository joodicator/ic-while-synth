module Logic where

import Clingo

type Condition = String

termToCond :: Clingo.Term -> Maybe Condition
termToCond (TStr cond) = Just cond
termToCond _           = Nothing
