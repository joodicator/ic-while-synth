{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, MultiParamTypeClasses,
             FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

module ASP.Base where

import Util

import qualified Data.Set as S
import Data.String
import Data.Monoid
import Data.Traversable
import Data.MonoTraversable
import Control.Applicative

--------------------------------------------------------------------------------
-- Various namespaces occurring in an Answer Set Program.

newtype Predicate = Predicate String deriving (IsString, Eq, Ord)
newtype Variable  = Variable  String deriving (IsString, Eq, Ord)
newtype Function  = Function  String deriving (IsString, Eq, Ord)
newtype Constant  = Constant  String deriving (IsString, Eq, Ord)
newtype LuaFunc   = LuaFunc   String deriving (IsString, Eq, Ord)

--------------------------------------------------------------------------------
-- The top-level types in an Answer Set Program.

infix 1 :-
data Rule = Head :- Body

newtype Head = Head [Atom] deriving Monoid

--------------------------------------------------------------------------------
-- Types occurring in ASP rule bodies.

newtype Body = Body [Conjunct] deriving Monoid

-- A conjunct in a rule body (or the RHS of a condition).
data Conjunct
  = CLiteral   Literal
  | CAssign    Variable Expr
  | CCondition Literal Conjunct

-- The positive or negative assertion of a predicate on some arguments.
data Literal
  = LCompare Comparison
  | LAtom    Atom
  | LNot     Atom
  deriving (Eq, Ord)

-- The application of a predicate to some arguments.
data Atom
  = Atom Predicate [Expr]
  deriving (Eq, Ord)

-- Comparison of term-expressions.
data Comparison
  = CBiOp CBiOp Expr Expr
  deriving (Eq, Ord)

-- Binary comparison predicates.
data CBiOp
  = CEq | CNE | CLT | CLE | CGT | CGE
  deriving (Eq, Ord)

-- Arithmetic expressions over terms.
data Expr
  = ETerm Term
  | EUnOp EUnOp Expr
  | EBiOp EBiOp Expr Expr
  deriving (Eq, Ord)

-- Binary arithmetic operators.
data EBiOp
  = EAdd | ESub | EMul | EDiv | EMod | EPow | EAnd | EOr | EXOr
  deriving (Eq, Ord, Enum, Bounded)

-- Unary arithmetic operators.
data EUnOp
  = ENeg | EAbs | ENot
  deriving (Eq, Ord, Enum, Bounded)

-- A /term/ which may occur as an argument to a predicate.
data Term
  = TVar Variable
  | TCon Constant
  | TInt Integer
  | TStr String
  | TFun Function [Expr]
  | TLua LuaFunc [Expr]
  deriving (Eq, Ord)

--------------------------------------------------------------------------------
-- Syntactic sugar for construction of ASP values.

-- Construction of predicate applications from string literals.
instance IsString ([Expr] -> Atom) where
    fromString = Atom . fromString
instance IsString ([Term] -> Atom) where
    fromString = (. map ETerm) . Atom . fromString
instance IsString ([a] -> Atom) => IsString ([a] -> Literal) where
    fromString = (LAtom .) . fromString
instance IsString ([a] -> Literal) => IsString ([a] -> Conjunct) where
    fromString = (CLiteral .) . fromString

-- Construction of nullary predicate applications from string literals.
instance IsString Atom where
    fromString = flip Atom [] . fromString
instance IsString Literal where
    fromString = LAtom . fromString
instance IsString Conjunct where
    fromString = CLiteral . fromString

-- Construction of function symbol applications from string literals.
instance IsString ([Expr] -> Term) where
    fromString = TFun . fromString
instance IsString ([Term] -> Term) where
    fromString = (. map ETerm) . TFun . fromString
instance IsString ([a] -> Term) => IsString ([a] -> Expr) where
    fromString = (ETerm .) . fromString

-- Construction of nullary function symbol applications from string literals.
instance IsString Term where
    fromString = flip TFun [] . fromString
instance IsString Expr where
    fromString = ETerm . fromString

-- Construction of assignment statements using the =: operator.
instance EqualsColon Variable Expr Conjunct where
    (=:) = CAssign

-- Construction of arithmetic expressions using standard operators.
instance Num Expr where
    (+)    = EBiOp EAdd
    (-)    = EBiOp ESub
    (*)    = EBiOp EMul
    negate = EUnOp ENeg
    abs    = EUnOp EAbs
    signum = error "signum not supported for ASP.Expr."
    fromInteger = ETerm . TInt

--------------------------------------------------------------------------------
-- MonoTraversable instances for traversal over the free variables occurring
-- in various structures.

newtype FreeVars a = FreeVars{ unFreeVars :: a }

type instance Element (FreeVars a) = Variable

freeVars :: MonoTraversable (FreeVars a) => a -> S.Set Variable
freeVars = S.fromList . otoList . FreeVars

isFreeIn :: MonoTraversable (FreeVars a) => Variable -> a -> Bool
isFreeIn var = oelem var . FreeVars

traverseFV
  :: (Applicative f, MonoTraversable (FreeVars a))
  => (Variable -> f Variable) -> a -> f a
traverseFV f = fmap unFreeVars . otraverse f . FreeVars 

instance MonoTraversableD (FreeVars Head) where
    otraverseD f (FreeVars (Head disjs))
      = FreeVars . Head <$> traverse (traverseFV f) disjs

instance MonoTraversableD (FreeVars Body) where
    otraverseD f (FreeVars (Body conjs))
      = FreeVars . Body <$> traverse (traverseFV f) conjs

instance MonoTraversableD (FreeVars Atom) where
    otraverseD f (FreeVars (Atom p xs))
      = FreeVars . Atom p <$> traverse (traverseFV f) xs

instance MonoTraversableD (FreeVars Conjunct) where
    otraverseD f (FreeVars conj) = FreeVars <$> case conj of
        CLiteral   lt    -> CLiteral   <$> traverseFV f lt
        CAssign    vr ex -> CAssign    <$> traverseFV f vr <*> traverseFV f ex
        CCondition lt cn -> CCondition <$> traverseFV f lt <*> traverseFV f cn

instance MonoTraversableD (FreeVars Literal) where
    otraverseD f (FreeVars lit) = FreeVars <$> case lit of
        LCompare cm -> LCompare <$> traverseFV f cm
        LAtom    at -> LAtom    <$> traverseFV f at
        LNot     at -> LNot     <$> traverseFV f at

instance MonoTraversableD (FreeVars Comparison) where
    otraverseD f (FreeVars comp) = FreeVars <$> case comp of
        CBiOp op x y -> CBiOp op <$> traverseFV f x <*> traverseFV f y

instance MonoTraversableD (FreeVars Expr) where
    otraverseD f (FreeVars expr) = FreeVars <$> case expr of
        ETerm    tm  -> ETerm    <$> traverseFV f tm
        EUnOp op x   -> EUnOp op <$> traverseFV f x
        EBiOp op x y -> EBiOp op <$> traverseFV f x <*> traverseFV f y

instance MonoTraversableD (FreeVars Term) where
    otraverseD f (FreeVars term) = FreeVars <$> case term of
        TVar vr -> TVar <$> traverseFV f vr
        _       -> pure term

instance MonoTraversableD (FreeVars Variable) where
    otraverseD f (FreeVars var) = FreeVars <$> f var

instance (Traversable t, MonoTraversable (FreeVars a))
      => MonoTraversableD (FreeVars (t a)) where
    otraverseD f (FreeVars t) = FreeVars <$> traverse (traverseFV f) t

--------------------------------------------------------------------------------
-- Negation instances for various structures.

instance Negation Literal where
    negation lit = case lit of
        LAtom a    -> LNot a
        LNot a     -> LAtom a
        LCompare c -> LCompare (negation c)

instance Negation Comparison where
    negation comp = case comp of
        CBiOp op x y -> CBiOp (negation op) x y

instance Negation CBiOp where
    negation op = case op of
        CEq -> CNE
        CNE -> CEq
        CLT -> CGE
        CGE -> CLT
        CGT -> CLE
        CLE -> CGT
