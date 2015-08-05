{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Logic where

import qualified Data.Set as S
import Data.Maybe

import Control.Monad
import Data.Foldable (Foldable)
import Data.Traversable

import Util

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
  deriving (Show, Functor, Foldable, Traversable)

instance Monad Prop where
    return          = PAtom
    PTrue     >>= _ = PTrue
    PFalse    >>= _ = PFalse
    PAtom a   >>= f = f a
    PAnd  p q >>= f = PAnd (p >>= f) (q >>= f)
    POr   p q >>= f = POr  (p >>= f) (q >>= f)
    PNot  p   >>= f = PNot (p >>= f)

instance Applicative Prop where
    pure  = return
    (<*>) = ap

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
pToDNF = pToDNF' LAtom

-- As pToDNF, but allows a custom type for literals, rather than Logic.Lit.
pToDNF' :: (Eq l, Ord l, Negation l) => (a -> l) -> Prop a -> Disj (Conj l)
pToDNF' mkLit prop = case prop of
    PTrue    -> Disj (S.singleton $ Conj S.empty)
    PFalse   -> Disj S.empty
    PAtom a  -> Disj (S.singleton $ Conj (S.singleton $ mkLit a))
    POr p q  -> Disj $ unDisj (pToDNF' mkLit p) `S.union`
                       unDisj (pToDNF' mkLit q)
    PAnd p q -> Disj . S.fromList $ do
        pc <- S.toList . unDisj $ pToDNF' mkLit p
        qc <- S.toList . unDisj $ pToDNF' mkLit q
        maybeToList $ mergeC pc qc
    PNot (PAtom a) -> Disj(S.singleton $ Conj(S.singleton . negation $ mkLit a))
    PNot p         -> pToDNF' mkLit $ negation p
  where
    -- Gives Just the union of two conjunctions of literals, or Nothing
    -- if both A and ¬A would occur in the result for any atom A.
    mergeC :: (Eq l, Ord l, Negation l) => Conj l -> Conj l -> Maybe (Conj l)
    mergeC (Conj xs) (Conj ys)
      | any (flip S.member zs . negation) (S.toList zs) = Nothing
      | otherwise                                       = Just (Conj zs)
      where zs = xs `S.union` ys

-- Negates a proposition P in such a way that its negation ¬P either:
-- 1. is a literal, i.e. is of the form ¬A or A for some atom A; or
-- 2. is _not_ of the form ¬P for any proposition P.
instance Negation (Prop a) where
    negation prop = case prop of
        PTrue    -> PFalse
        PFalse   -> PTrue
        PAtom a  -> PNot (PAtom a)
        PAnd p q -> POr  (PNot p) (PNot q)
        POr  p q -> PAnd (PNot p) (PNot q)
        PNot p   -> p

instance Negation (Lit a) where
    negation lit = case lit of
        LAtom a -> LNot  a
        LNot  a -> LAtom a
