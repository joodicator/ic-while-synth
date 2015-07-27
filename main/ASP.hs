{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}

module ASP(
    Predicate(..), Variable(..), Function(..), Constant(..), LuaFunc(..),
    Rule(..), Head(..), Body(..), Conjunct(..), Literal(..),
    Atom(..), Comparison(..), Expr(..), Term(..), CBiOp(..), EBiOp(..), EUnOp(..),
    propToBodies, propToRules
) where

import qualified Logic
import Util

import qualified Data.Set as S
import Data.List
import Data.Char
import Data.String
import Data.Monoid

--------------------------------------------------------------------------------
-- Various namespaces occurring in an Answer Set Program.

newtype Predicate = Predicate String deriving (IsString, Eq, Ord)
newtype Variable  = Variable  String deriving (IsString, Eq, Ord)
newtype Function  = Function  String deriving (IsString, Eq, Ord)
newtype Constant  = Constant  String deriving (IsString, Eq, Ord)
newtype LuaFunc   = LuaFunc   String deriving (IsString, Eq, Ord)

--------------------------------------------------------------------------------
-- The top-level types in an Answer Set Program.

data Rule = Rule Head Body

newtype Head = Head [Atom]

--------------------------------------------------------------------------------
-- Types occurring in ASP rule bodies.

newtype Body
  = Body [Conjunct]
  deriving Monoid

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
  deriving (Eq, Ord)

-- Unary arithmetic operators.
data EUnOp
  = ENeg | EAbs | ENot
  deriving (Eq, Ord)

-- A /term/ which may occur as an argument to a predicate.
data Term
  = TVar Variable
  | TCon Constant
  | TInt Integer
  | TFun Function [Expr]
  | TLua LuaFunc [Expr]
  deriving (Eq, Ord)

--------------------------------------------------------------------------------
-- Conversion from classical propositions.

-- Given a rule head H and a classical proposition P in ASP literals, and a
-- possibly empty body fragment fixing the domain for each variable for which
-- this is necessary, gives a set of rules equivalent to H :- P, under the
-- assumption that negation in P is equivalent to negation-as-failure and not
-- classical negation in ASP's sense.
propToRules :: Head -> (Variable -> Body) -> Logic.Prop Literal -> [Rule]
propToRules head dom prop
  = undefined
    
-- Given a classical proposition in ASP literals, gives a disjunctive list of
-- rule bodies equivalent to the proposition in the same sense as propToRules.
propToBodies :: Logic.Prop Literal -> [Body]
propToBodies prop
  = map disj . S.toList . Logic.unDisj $ Logic.pToDNF prop
  where
    disj (Logic.Conj cs)   = Body . map conj $ S.toList cs
    conj (Logic.LAtom lit) = CLiteral lit
    conj (Logic.LNot  lit) = CLiteral (negateLiteral lit)

--------------------------------------------------------------------------------
-- Miscellaneous utilities.

negateLiteral :: Literal -> Literal
negateLiteral lit = case lit of
    LAtom a    -> LNot a
    LNot a     -> LAtom a
    LCompare c -> LCompare (negateComparison c)

negateComparison :: Comparison -> Comparison
negateComparison comp = case comp of
    CBiOp op x y -> CBiOp (negateCBiOp op) x y

negateCBiOp :: CBiOp -> CBiOp
negateCBiOp op = case op of
    CEq -> CNE
    CNE -> CEq
    CLT -> CGE
    CGE -> CLT
    CGT -> CLE
    CLE -> CGT

--------------------------------------------------------------------------------
-- Information about ASP operators.

data ASPContext = Clingo3

instance Operator EBiOp where
    type OpContext EBiOp = ASPContext
    opInfo cxt@Clingo3 op = case op of
        EXOr -> OpInfo{ oPrec=1, oAscL=True, oAscR=True }
        EOr  -> OpInfo{ oPrec=2, oAscL=True, oAscR=True }
        EAnd -> OpInfo{ oPrec=3, oAscL=True, oAscR=True }
        EAdd -> OpInfo{ oPrec=4, oAscL=True, oAscR=True }
        ESub -> OpInfo{ oPrec=4, oAscL=True, oAscR=False }
        EMul -> OpInfo{ oPrec=5, oAscL=True, oAscR=True }
        EDiv -> OpInfo{ oPrec=5, oAscL=True, oAscR=False }
        EMod -> OpInfo{ oPrec=5, oAscL=True, oAscR=False }
        EPow -> OpInfo{ oPrec=6, oAscL=False, oAscR=True }

instance Operator EUnOp where
    type OpContext EUnOp = ASPContext
    opInfo cxt@Clingo3 op = case op of
        ENeg -> OpInfo{ oPrec=7, oAscL=False, oAscR=True }
        ENot -> OpInfo{ oPrec=7, oAscL=False, oAscR=True }
        EAbs -> OpInfo{ oPrec=9, oAscL=False, oAscR=False }

instance Operator Expr where
    type OpContext Expr = ASPContext
    opInfo cxt@Clingo3 expr = case expr of
        ETerm _      -> OpInfo{ oPrec=8, oAscL=False, oAscR=False }
        EUnOp op _   -> opInfo cxt op
        EBiOp op _ _ -> opInfo cxt op

--------------------------------------------------------------------------------
-- Show instances for ASP-syntax string representations.

instance Show Predicate where
    show (Predicate p)
      | any isLower (take 1 p) = p
      | otherwise = error $ "\""++ p ++"\" is not a valid ASP predicate name."

instance Show Variable where
    show (Variable v)
      | any isUpper (take 1 v) = v
      | otherwise = error $ "\""++ v ++"\" is not a valid ASP variable."

instance Show Constant where
    show (Constant c)
      | any isLower (take 1 c) = c
      | otherwise = error $ "\""++ c ++ "\" is not a valid ASP constant."

instance Show Function where
    show (Function f)
      | any isLower (take 1 f) = f
      | otherwise = error $ "\""++ f ++ "\" is not a valid ASP function symbol."

instance Show LuaFunc where
    show (LuaFunc f)
      | not (null f) = "@" ++ f
      | otherwise = error $ "\""++ f ++ "\" is not a valid Lua function name."

instance Show Rule where
    show (Rule (Head []) body) = ":- " ++ show body ++ "."
    show (Rule head (Body [])) = show head ++ "."
    show (Rule head body)      = show head ++ " :- " ++ show body ++ "."

instance Show Head where
    show (Head ds) = intercalate " | " $ map show ds

instance Show Body where
    show (Body cs) = intercalate ", " $ map show cs

instance Show Atom where
    show (Atom p xs) = show p ++ showArgs xs

instance Show Conjunct where
    show conj = case conj of
        CLiteral lit         -> show lit
        CAssign var expr     -> show var ++"="++ show expr
        CCondition head tail -> show head ++" : "++ show tail

instance Show Literal where
    show conj = case conj of
        LCompare comp -> show comp
        LAtom atom    -> show atom
        LNot  atom    -> "not "++ show atom

instance Show Comparison where
    show comp = case comp of
        CBiOp op x y -> show x ++ show op ++ show y

instance Show CBiOp where
    show op = case op of
        CEq -> " == "
        CNE -> " != "
        CLT -> " < "
        CLE -> " <= "
        CGT -> " > "
        CGE -> " >= "

instance Show Expr where
    show = showExpr 0

showExpr :: Prec -> Expr -> String 
showExpr pPrec expr = case expr of
    _ | prec <= pPrec -> "("++ show expr ++")"
    ETerm t           -> show t
    EBiOp op x y      -> showL x ++ show op ++ showR y
    EUnOp EAbs x      -> "|" ++ show x ++ "|"
    EUnOp op x        -> show op ++ showR x
  where
    OpInfo{ oPrec=prec, oAscL=ascL, oAscR=ascR } = opInfo Clingo3 expr
    showL = showExpr $ if ascL then prec-1 else prec
    showR = showExpr $ if ascR then prec-1 else prec

showArgs :: [Expr] -> String
showArgs xs = "("++ intercalate "," (map show xs) ++")"

instance Show EBiOp where
    show op = case op of
        EXOr -> "^"
        EOr  -> "?"
        EAnd -> "&"
        EAdd -> "+"
        ESub -> "-"
        EMul -> "*"
        EDiv -> "/"
        EMod -> "\\"
        EPow -> "**"

instance Show EUnOp where
    show op = case op of
        ENeg -> "-"
        ENot -> "~"
        EAbs -> "#abs"

instance Show Term where
    show term = case term of
        TVar v    -> show v
        TCon c    -> show c
        TInt n    -> show n
        TFun f xs -> show f ++ showArgs xs
        TLua f xs -> show f ++ showArgs xs
