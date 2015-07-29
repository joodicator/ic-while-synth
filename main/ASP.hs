{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies,
             FlexibleInstances, FlexibleContexts #-}

module ASP(
    Predicate(..), Variable(..), Function(..), Constant(..), LuaFunc(..),
    Rule(..), Head(..), Body(..), Conjunct(..), Literal(..),
    Atom(..), Comparison(..), Expr(..), Term(..), CBiOp(..), EBiOp(..), EUnOp(..),
    FreeVars(..),
    propToRules
) where

import qualified Logic
import Util

import qualified Data.Set as S
import Data.List
import Data.Char
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

data Rule = Rule Head Body

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

instance IsString ([Expr] -> Literal) where
    fromString = (LAtom .) . fromString

-- The application of a predicate to some arguments.
data Atom
  = Atom Predicate [Expr]
  deriving (Eq, Ord)

instance IsString ([Expr] -> Atom) where
    fromString = Atom . fromString

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

instance IsString Expr where
    fromString = ETerm . fromString

instance Num Expr where
    (+)    = EBiOp EAdd
    (-)    = EBiOp ESub
    (*)    = EBiOp EMul
    negate = EUnOp ENeg
    abs    = EUnOp EAbs
    signum = error "signum not supported for ASP.Expr."
    fromInteger = ETerm . TInt

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

instance IsString ([Expr] -> Term) where
    fromString = TFun . fromString

instance IsString Term where
    fromString = flip TFun [] . fromString

--------------------------------------------------------------------------------
-- Conversion from classical propositions.

-- Given a rule head H and a classical proposition P in ASP literals, and a
-- possibly empty body fragment fixing the domain for each variable for which
-- this is necessary, gives a set of rules equivalent to H :- P, under the
-- assumption that negation in P is equivalent to negation-as-failure and not
-- classical negation in ASP's sense.
propToRules :: Head -> (Variable -> Body) -> Logic.Prop Literal -> [Rule]
propToRules rHead dom
  = map bodyToRule . propToBodies
  where
    bodyToRule rBody
      = Rule rHead $ rBody <> ofoldMap dom (headVars `S.union` bodyVars)
      where bodyVars = freeVars rBody
    headVars = freeVars rHead
  
-- Given a classical proposition in ASP literals, gives a disjunctive list of
-- rule bodies equivalent to the proposition in the same sense as propToRules.
propToBodies :: Logic.Prop Literal -> [Body]
propToBodies prop
  = map disj . S.toList . Logic.unDisj $ Logic.pToDNF' id prop
  where
    disj (Logic.Conj cs) = Body . map CLiteral $ S.toList cs

--------------------------------------------------------------------------------
-- MonoTraversable instances for traversal over the free variables occurring
-- in various structures.

newtype FreeVars a = FreeVars{ unFreeVars :: a }

type instance Element (FreeVars a) = Variable

freeVars :: MonoTraversable (FreeVars a) => a -> S.Set Variable
freeVars = S.fromList . otoList . FreeVars

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

--------------------------------------------------------------------------------
-- Information about ASP operators.

data ASPContext = Clingo3

instance Operator EBiOp where
    type OpContext EBiOp = ASPContext
    opInfo Clingo3 op = case op of
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
    opInfo Clingo3 op = case op of
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
    show (Rule (Head []) rBody) = ":- " ++ show rBody ++ "."
    show (Rule rHead (Body [])) = show rHead ++ "."
    show (Rule rHead rBody)     = show rHead ++ " :- " ++ show rBody ++ "."

instance Show Head where
    show (Head ds) = intercalate " | " $ map show ds

instance Show Body where
    show (Body cs) = intercalate ", " $ map show cs

instance Show Atom where
    show (Atom p xs) = show p ++ showArgs xs

instance Show Conjunct where
    show conj = case conj of
        CLiteral lit           -> show lit
        CAssign var expr       -> show var ++"="++ show expr
        CCondition cHead cTail -> show cHead ++" : "++ show cTail

instance Show Literal where
    show conj = case conj of
        LCompare comp -> show comp
        LAtom atom    -> show atom
        LNot  atom    -> "not "++ show atom

instance Show Comparison where
    show comp = case comp of
        CBiOp CGT x y -> show $ CBiOp CLT y x
        CBiOp CGE x y -> show $ CBiOp CLE y x
        CBiOp op  x y -> show x ++ show op ++ show y

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
