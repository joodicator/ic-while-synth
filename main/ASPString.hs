{-# LANGUAGE ViewPatterns, TypeFamilies #-}

module ASPString where

import Data.Char
import Data.Maybe
import Data.List

import Data.Traversable
import Data.MonoTraversable
import Control.Applicative

import Clingo
import While
import Util

--------------------------------------------------------------------------------
-- Conditions in the same syntax of ASP rule bodies, taking the form of Strings.
--------------------------------------------------------------------------------

type Variable  = String
type Condition = String
type Lexeme    = String

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
    GNeg guard' -> guardToCond (negation guard')

exprToCond :: Expr -> Condition
exprToCond expr = case expr of
    ECon c        -> show c
    EVar (Name v) -> headMap toUpper v
    _             -> error "exprToCond: arithmetic not implemented"

--------------------------------------------------------------------------------
-- Traversal of various components of Conditions.

traverseFreeVariables
  :: Applicative f => (Variable -> f Variable) -> Condition -> f Condition
traverseFreeVariables act
  = traverseLexemes $ \lexeme -> case lexeme of
        c:_ | isUpper c -> act  lexeme
        _               -> pure lexeme

traverseConjuncts
  :: Applicative f => (Condition -> f Condition) -> Condition -> f Condition
traverseConjuncts act
  = fmap reassemble . traverse act . map trim . conjuncts . otoList . Lexemes
  where
    reassemble = intercalate ", " . filter (not . all isSpace)
    trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse
    conjuncts :: [Lexeme] -> [Condition]
    conjuncts xs = case xs of
        "(" : (endParen -> (h, conjuncts -> h' : t)) -> ('(':h ++ h') : t
        "," : (conjuncts -> t)                       -> ""            : t
        x   : (conjuncts -> h : t)                   -> (x ++ h)      : t
        []                                           -> [""]
        _                                            -> error "impossible"
    endParen :: [Lexeme] -> (String, [Lexeme])
    endParen xs = case xs of
        "(" : (endParen -> (h, t)) -> ('(':h, t)
        ")" : xs'                  -> (")",   xs')
        x   : (endParen -> (h, t)) -> (x++h,  t)
        _                          -> error "impossible"

traverseLexemes
  :: Applicative f => (String -> f String) -> Condition -> f Condition
traverseLexemes act cond = case cond of
    (span isSpace -> (space@(_:_), cTail)) ->
        -- One or more spaces.
        (++) <$> act space <*> traverseLexemes act cTail
    (span isDigit -> (digits@(_:_), cTail)) ->
        -- One or more digits.
        (++) <$> act digits <*> traverseLexemes act cTail
    (span (\c -> isAlphaNum c || c == '_') -> (name@(_:_), cTail)) ->
        -- One or more alphanumeric or underscore characters.
        (++) <$> act name <*> traverseLexemes act cTail
    '#' : (span isAlpha -> (word, cTail)) ->
        -- A word starting with #.
        (++) <$> act ('#' : word) <*> traverseLexemes act cTail
    '"' : (str -> (string, cTail)) ->
        -- A string literal.
        (++) <$> act ('"' : string) <*> traverseLexemes act cTail
    '%':'*' : (com -> (comment, cTail)) ->
        -- An inline comment.
        (++) <$> act ('%':'*': comment) <*> traverseLexemes act cTail
    '%' : comment ->
        -- A comment extending to the end of the line.
        act ('%' : comment)
    (sym -> Just (symbol, cTail)) ->
        -- A symbol of two or more characters.
        (++) <$> act symbol <*> traverseLexemes act cTail
    symbol : cTail ->
        -- A symbol of one character.
        (++) <$> act [symbol] <*> traverseLexemes act cTail
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
        p <- [":-", "<=", ">=", "!=", ".."]
        s' <- maybeToList $ stripPrefix p s
        return (p, s')

-- The following functions are kept for (at least) backward-compatibility:

mapFreeVariables :: (Variable -> Variable) -> Condition -> Condition
mapFreeVariables f
  = unVariables . omap f . Variables

subFreeVariable :: Variable -> Variable -> Condition -> Condition
subFreeVariable v v'
  = subFreeVariables [(v, v')]

subFreeVariables :: [(Variable, Variable)] -> Condition -> Condition
subFreeVariables subs
  = mapFreeVariables $ \v -> fromMaybe v $ lookup v subs'
  where subs' = [(headUp v, headUp v') | (v, v') <- subs]

freeVariables :: Condition -> [Variable]
freeVariables
  = nub . sort . otoList . Variables

isFreeIn :: Variable -> Condition -> Bool
isFreeIn var
  = oany (== (headUp var)) . Variables

--------------------------------------------------------------------------------
-- MonoTraversable instances for Condition traversals.

newtype Lexemes   = Lexemes   { unLexemes   :: Condition }
newtype Variables = Variables { unVariables :: Condition }
newtype Conjuncts = Conjuncts { unConjuncts :: Condition }

type instance Element Lexemes   = String
type instance Element Variables = Variable
type instance Element Conjuncts = Condition

instance MonoTraversableD Lexemes where
    otraverseD act
      = fmap Lexemes . traverseLexemes act . unLexemes

instance MonoTraversableD Variables where
    otraverseD act
      = fmap Variables . traverseFreeVariables act . unVariables

instance MonoTraversableD Conjuncts where
    otraverseD act
      = fmap Conjuncts . traverseConjuncts act . unConjuncts
