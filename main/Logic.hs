{-# LANGUAGE ViewPatterns, TypeFamilies, DeriveFunctor,
             FlexibleInstances, UndecidableInstances #-}

module Logic where

import Data.Char
import Data.Maybe
import Data.List
import Data.Maybe
import Data.Functor.Identity
import qualified Data.Set as S

import Data.Monoid
import Data.Traversable
import Data.MonoTraversable
import Control.Applicative

import Clingo
import While

--------------------------------------------------------------------------------
-- Logical propositions represented classically.
--------------------------------------------------------------------------------

data Prop a
  = PTrue
  | PFalse
  | PAtom a
  | PAnd (Prop a) (Prop a)
  | POr  (Prop a) (Prop a)
  | PNot (Prop a)
  deriving (Show, Functor)

data Lit a
  = LAtom a | LNot a
  deriving (Eq, Ord, Show)

newtype Conj a
  = Conj{ unConj :: S.Set a }
  deriving (Eq, Ord, Show)

newtype Disj a
  = Disj{ unDisj :: S.Set a }
  deriving (Eq, Ord, Show)

type DNF a = Disj (Conj (Lit a))

--------------------------------------------------------------------------------
-- Gives a proposition in /disjunctive normal form/, i.e. as a disjunction of
-- conjunctions of /literals/, i.e. of atoms and negated atoms.
pToDNF :: (Eq a, Ord a) => Prop a -> DNF a
pToDNF prop = case prop of
    PTrue      -> Disj S.empty
    PFalse     -> Disj (S.singleton $ Conj S.empty)
    (PAtom a)  -> Disj (S.singleton $ Conj (S.singleton $ LAtom a))
    (POr p q)  -> Disj $ unDisj (pToDNF p) `S.union` unDisj (pToDNF q)
    (PAnd p q) -> Disj $ S.fromList [c | pc <- S.toList . unDisj $ pToDNF p,
                                         qc <- S.toList . unDisj $ pToDNF q,
                                         c <- maybeToList $ mergeC pc qc]
    (PNot (PAtom a)) -> Disj (S.singleton $ Conj (S.singleton $ LNot a))
    (PNot p)         -> pToDNF $ pNegate p
  where
    -- Gives Just the union of two conjunctions of literals, or Nothing
    -- if both A and ¬A would occur in the result for any atom A.
    mergeC :: (Eq a, Ord a) => Conj(Lit a) -> Conj(Lit a) -> Maybe(Conj(Lit a))
    mergeC (Conj xs) (Conj ys)
      | any (flip S.member zs . lNegate) (S.toList zs) = Nothing
      | otherwise                                      = Just (Conj zs)
      where zs = xs `S.union` ys

-- Negates a proposition P in such a way that its negation ¬P either:
-- 1. is a literal, i.e. is of the form ¬A or A for some atom A; or
-- 2. is _not_ of the form ¬P for any proposition P.
pNegate :: Prop a -> Prop a
pNegate PTrue      = PFalse
pNegate PFalse     = PTrue
pNegate (PAtom a)  = PNot (PAtom a)
pNegate (PAnd p q) = POr  (PNot p) (PNot q)
pNegate (POr  p q) = PAnd (PNot p) (PNot q)
pNegate (PNot p)   = p

-- Negate a literal.
lNegate :: Lit a -> Lit a
lNegate (LAtom a) = (LNot  a)
lNegate (LNot  a) = (LAtom a)

--------------------------------------------------------------------------------
-- Logical propositions represented as ASP rule bodies.
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
    GNeg guard' -> guardToCond (negateGuard guard')

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
        ")" : xs                   -> (")",   xs)
        x   : (endParen -> (h, t)) -> (x++h,  t)
        _                          -> error "impossible"

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
        -- A symbol of two or more characters.
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
        p <- [":-", "<=", ">=", "!=", ".."]
        s <- maybeToList $ stripPrefix p s
        return (p, s)

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

