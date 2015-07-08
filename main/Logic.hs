module Logic where

import Data.Monoid
import Data.Functor.Identity
import Data.Char
import Data.List
import Control.Applicative

import Clingo
import While

--------------------------------------------------------------------------------
type Variable  = String
type Condition = String

--------------------------------------------------------------------------------
-- Conversion to and from Conditions.
termToCond :: Term -> Maybe Condition
termToCond (TStr cond) = Just cond
termToCond _           = Nothing

guardToCond :: Guard -> Condition
guardToCond guard = case guard of
    GLT e1 e2   -> exprToCond e1 ++ "<"  ++ exprToCond e2
    GLE e1 e2   -> exprToCond e1 ++ "<=" ++ exprToCond e2
    GGT e1 e2   -> exprToCond e1 ++ ">"  ++ exprToCond e2
    GGE e1 e2   -> exprToCond e1 ++ ">=" ++ exprToCond e2
    GEQ e1 e2   -> exprToCond e1 ++ "==" ++ exprToCond e2
    GNE e1 e2   -> exprToCond e1 ++ "!=" ++ exprToCond e2
    GNeg guard' -> guardToCond (negateGuard guard')

exprToCond :: Expr -> Condition
exprToCond expr = case expr of
    ECon c        -> show c
    EVar (Name v) -> headMap toUpper v
    _             -> error "exprToCond: arithmetic not implemented"

--------------------------------------------------------------------------------
-- Traversal of free variables occurring in conditions.
traverseFreeVariables
  :: Applicative f => (Variable -> f Variable) -> Condition -> f Condition
traverseFreeVariables act cond@(c : _) | isVariableHead c
  = (++) <$> act var <*> traverseFreeVariables act tail
  where
    (var, tail) = span isVariableBody cond
traverseFreeVariables act cond@(_ : _)
  = (head ++) <$> traverseFreeVariables act tail
  where (head, tail) = break isVariableHead cond
traverseFreeVariables _ ""
  = pure ""

isVariableHead c = isUpper c
isVariableBody c = isAlphaNum c || c == '_'

mapFreeVariables :: (Variable -> Variable) -> Condition -> Condition
mapFreeVariables f cond 
  = runIdentity $ traverseFreeVariables (pure . f) cond

subFreeVariable :: Variable -> Variable -> Condition -> Condition
subFreeVariable v v'
  = mapFreeVariables $ \u -> if u==v then v' else u

freeVariables :: Condition -> [Variable]
freeVariables cond
  = nub . sort $ appEndo prependVars []
  where
    prependVars = getConst . traverseFreeVariables (Const . Endo . (:)) $ cond

isFreeIn :: Variable -> Condition -> Bool
isFreeIn var = elem var . freeVariables

--------------------------------------------------------------------------------
-- Miscellaneous utilities.
headMap :: (a -> a) -> [a] -> [a]
headMap f (x : xs) = f x : xs
headMap _ []       = []

headUp  = headMap toUpper
headLow = headMap toLower

