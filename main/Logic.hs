{-# LANGUAGE ViewPatterns, TypeFamilies,
             FlexibleInstances, UndecidableInstances #-}

module Logic where

import Data.Char
import Data.Maybe
import Data.List
import Data.Functor.Identity

import Data.Monoid
import Data.MonoTraversable
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
-- Traversal of various components of Conditions.

traverseLexemes
  :: Applicative f => (String -> f String) -> Condition -> f Condition
traverseLexemes act cond = case cond of
    (span isSpace -> (space@(_:_), tail)) ->
        -- One or more spaces.
        (++) <$> act space <*> traverseLexemes act tail
    (span isDigit -> (digits@(_:_), tail)) ->
        -- One or more digits.
        (++) <$> act digits <*> traverseLexemes act tail
    (span (\c -> isAlphaNum c || c == '_') -> (name@(_:_), tail)) ->
        -- One or more alphanumeric or underscore characters.
        (++) <$> act name <*> traverseLexemes act tail
    '#' : (span isAlpha -> (word, tail)) ->
        -- A word starting with #.
        (++) <$> act ('#' : word) <*> traverseLexemes act tail
    '"' : (str -> (string, tail)) ->
        -- A string literal.
        (++) <$> act ('"' : string) <*> traverseLexemes act tail
    '%':'*' : (com -> (comment, tail)) ->
        -- An inline comment.
        (++) <$> act ('%':'*': comment) <*> traverseLexemes act tail
    '%' : comment ->
        -- A comment extending to the end of the line.
        act ('%' : comment)
    (sym -> Just (symbol, tail)) ->
        -- An symbol of two or more characters.
        (++) <$> act symbol <*> traverseLexemes act tail
    symbol : tail ->
        -- A symbol of one character.
        (++) <$> act [symbol] <*> traverseLexemes act tail
    "" -> pure ""
  where
    str s = case s of
        '"'   : cs             -> ('"':"",  cs)
        '\\':c : (str -> (h,t)) -> ('\\':c:h, t)
        c     : (str -> (h,t)) -> (c:h,     t)
        ""                       -> ("",      "")
    com s = case s of
        '*':'%' : cs                          -> ("*%",        cs)
        '%':'*' : (com -> (h, com -> (h',t))) -> ("%*"++h++h', t)
        c       : (com -> (h,t))              -> (c:h,         t)
        ""                                    -> ("",          "")
    sym s = listToMaybe $ do
        p <- [":-", "<=", ">=", "!="]
        s <- maybeToList $ stripPrefix p s
        return (p, s)

traverseFreeVariables
  :: Applicative f => (Variable -> f Variable) -> Condition -> f Condition
traverseFreeVariables act
  = traverseLexemes $ \lexeme -> case lexeme of
        var@((isUpper -> True) : _) -> act lexeme
        _                           -> pure lexeme

traverseConjuncts
  :: Applicative f => (Condition -> f Condition) -> Condition -> f Condition
traverseConjuncts act
  = fmap (intercalate ", ") . traverse act . conjuncts . otoList . Lexemes
  where
    conjuncts = map trim . conjuncts'
    trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse
    conjuncts' xs = case xs of
        "(" : (endParen -> (h, conjuncts' -> h' : t)) -> (h ++ h') : t
        "," : (conjuncts' -> t)                       -> ""        : t
        x   : (conjuncts' -> h : t)                   -> (x ++ h)  : t
        []                                            -> [""]
    endParen xs = case xs of
        "(" : (endParen -> (h, t)) -> ('(':h, t)
        ")" : xs                   -> (")",   xs)

-- The following functions are kept for (at least) backward-compatibility:

mapFreeVariables :: (Variable -> Variable) -> Condition -> Condition
mapFreeVariables f
  = unVariables . omap f . Variables

subFreeVariable :: Variable -> Variable -> Condition -> Condition
subFreeVariable v v'
  = unVariables . omap (\u -> if u==v then v' else u) . Variables

freeVariables :: Condition -> [Variable]
freeVariables
  = otoList . Variables

isFreeIn :: Variable -> Condition -> Bool
isFreeIn var
  = oany (== var) . Variables

--------------------------------------------------------------------------------
-- MonoTraversable instances for Condition traversals.
newtype Lexemes   = Lexemes   { unLexemes   :: Condition }
newtype Variables = Variables { unVariables :: Condition }
newtype Conjuncts = Conjuncts { unConjuncts :: Condition }

type instance Element Lexemes   = String
type instance Element Variables = Variable
type instance Element Conjuncts = Condition

instance MonoTraversableDefault Lexemes where
    otraverseDefault act
      = fmap Lexemes . traverseLexemes act . unLexemes

instance MonoTraversableDefault Variables where
    otraverseDefault act
      = fmap Variables . traverseFreeVariables act . unVariables

instance MonoTraversableDefault Conjuncts where
    otraverseDefault act
      = fmap Conjuncts . traverseConjuncts act . unConjuncts

--------------------------------------------------------------------------------
-- Default implementation of Mono{Traversable,Foldable,Functor}.

class MonoTraversableDefault mono where
    otraverseDefault :: Applicative f =>
        (Element mono -> f (Element mono)) -> mono -> f mono

instance MonoTraversableDefault mono => MonoTraversable mono where
    otraverse = otraverseDefault
    omapM act = unwrapMonad . otraverse (WrapMonad . act)

instance MonoTraversableDefault mono => MonoFoldable mono where
    ofoldMap f    = getConst . otraverseDefault (Const . f)
    ofoldr  f z t = appEndo (ofoldMap (Endo . f) t) z
    ofoldl' f     = ofoldlDefault $ \l r -> let l' = f l r in l' `seq` l'

    ofoldr1Ex f t
      = fromMaybe (error "ofoldr1Ex: empty") (ofoldr mf Nothing t)
      where mf l Nothing  = Just l
            mf l (Just r) = Just (f l r)

    ofoldl1Ex' f t
      = fromMaybe (error "ofoldlEx': empty") (ofoldlDefault mf Nothing t)
      where mf Nothing  r = Just r
            mf (Just l) r = l' `seq` Just l' where l' = f l r

ofoldlDefault :: MonoFoldable mono => (a -> Element mono -> a) -> a -> mono -> a
ofoldlDefault f z t = appEndo (getDual (ofoldMap (Dual . Endo . flip f) t)) z

instance MonoTraversableDefault mono => MonoFunctor mono where
    omap f = runIdentity . otraverse (Identity . f)

--------------------------------------------------------------------------------
-- Miscellaneous utilities.
headMap :: (a -> a) -> [a] -> [a]
headMap f (x : xs) = f x : xs
headMap _ []       = []

headUp  = headMap toUpper
headLow = headMap toLower

