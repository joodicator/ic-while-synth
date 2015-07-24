{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ASP where

import Data.String

newtype Predicate = Predicate String deriving IsString
newtype Variable  = Variable  String deriving IsString
newtype Function  = Function  String deriving IsString
newtype Constant  = Constant  String deriving IsString
newtype LuaFunc   = LuaFunc   String deriving IsString

newtype Body = Body [Conjunct]

-- A conjunct in a rule body (or the RHS of a condition).
data Conjunct
  = CSimple    SimpleConjunct
  | CCondition SimpleConjunct Conjunct

-- A non-condition conjunct in a rule body (or the LHS of a condition).
data SimpleConjunct
  = CAssign  Variable Expr
  | CCompare Comparison
  | CAtom    Atom
  | CNotAtom Atom

-- The application of a predicate to some arguments.
data Atom = Atom Predicate [Expr]

-- Comparison of term-expressions.
data Comparison = CBiOp CBiOp Expr Expr

-- Binary comparison predicates.
data CBiOp = CEq | CNE | CLT | CLE | CGT | CGE

-- Arithmetic expressions over terms.
data Expr
  = ETerm Term
  | EUnOp EUnOp Expr
  | EBiOp EBiOp Expr Expr

-- Binary arithmetic operators.
data EBiOp = EAdd | ESub | EMul | EDiv | EMod | EPow | EAnd | EOr | EXOr

-- Unary arithmetic operators.
data EUnOp = ENeg | EAbs | ENot

-- A /term/ which may occur as an argument to a predicate.
data Term
  = TVar Variable
  | TCon Constant
  | TInt Integer
  | TFun Function [Expr]
  | TLua LuaFunc [Expr]
