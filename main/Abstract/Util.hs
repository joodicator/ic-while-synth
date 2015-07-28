{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Abstract.Util(
    HsExpr(..), HsVar(..), HsInput(..),
    haskellToBool, boolToPropASP
) where

--------------------------------------------------------------------------------
-- Utilities for the module Abstract, that we wish to define in the presence
-- of the standard Prelude bindings.

import qualified Abstract.Base as A
import qualified ASP
import qualified Logic
import Util

import Language.Haskell.Interpreter

import Control.Applicative
import Data.List

--------------------------------------------------------------------------------
-- Information about operators in various contexts.

data Expr = I A.Int | B A.Bool

data ExprContext = Haskell

instance Operator Expr where
    type OpContext Expr = ExprContext
    opInfo cxt (I int)  = opInfo cxt int
    opInfo cxt (B bool) = opInfo cxt bool

instance Operator A.Int where
    type OpContext A.Int = ExprContext
    opInfo Haskell int = case int of
        A.ICon _      -> OpInfo{ oPrec=10, oAscL=False, oAscR=False }
        A.IVar _      -> OpInfo{ oPrec=10, oAscL=False, oAscR=False }
        A.IIf _ _ _   -> OpInfo{ oPrec=1, oAscL=False, oAscR=True }
        A.IIBi op _ _ -> opInfo Haskell op
        A.IIUn op _   -> opInfo Haskell op

instance Operator A.IIBi where
    type OpContext A.IIBi = ExprContext
    opInfo Haskell op = case op of
        A.IAdd -> OpInfo{ oPrec=5, oAscL=True, oAscR=True }
        A.ISub -> OpInfo{ oPrec=5, oAscL=True, oAscR=False }
        A.IMul -> OpInfo{ oPrec=6, oAscL=True, oAscR=True }
        A.IDiv -> OpInfo{ oPrec=6, oAscL=True, oAscR=False }
        A.IMod -> OpInfo{ oPrec=6, oAscL=True, oAscR=False }
        A.IPow -> OpInfo{ oPrec=7, oAscL=False, oAscR=True }

instance Operator A.IIUn where
    type OpContext A.IIUn = ExprContext
    opInfo Haskell op = case op of
        A.INeg -> OpInfo{ oPrec=5, oAscL=False, oAscR=False }
        A.IAbs -> OpInfo{ oPrec=9, oAscL=False, oAscR=False }

instance Operator A.Bool where
    type OpContext A.Bool = ExprContext
    opInfo Haskell bool = case bool of
        A.True        -> OpInfo{ oPrec=10, oAscL=False, oAscR=False }
        A.False       -> OpInfo{ oPrec=10, oAscL=False, oAscR=False }
        A.BBBi op _ _ -> opInfo Haskell op
        A.BBUn op _   -> opInfo Haskell op
        A.BInt bi     -> opInfo Haskell bi

instance Operator A.BoolInt where
    type OpContext A.BoolInt = ExprContext
    opInfo Haskell bool = case bool of
        A.BIBi op _ _ -> opInfo Haskell op

instance Operator A.BBBi where
    type OpContext A.BBBi = ExprContext
    opInfo Haskell op = case op of
        A.BAnd -> OpInfo{ oPrec=3, oAscL=True, oAscR=True }
        A.BOr  -> OpInfo{ oPrec=2, oAscL=True, oAscR=True }

instance Operator A.BBUn where
    type OpContext A.BBUn = ExprContext
    opInfo Haskell op = case op of
        A.BNot -> OpInfo{ oPrec=9, oAscL=False, oAscR=False }

instance Operator A.BIBi where
    type OpContext A.BIBi = ExprContext
    opInfo Haskell op = case op of
        A.BLT -> OpInfo{ oPrec=4, oAscL=False, oAscR=False }
        A.BGT -> OpInfo{ oPrec=4, oAscL=False, oAscR=False }
        A.BLE -> OpInfo{ oPrec=4, oAscL=False, oAscR=False }
        A.BGE -> OpInfo{ oPrec=4, oAscL=False, oAscR=False }
        A.BEq -> OpInfo{ oPrec=4, oAscL=False, oAscR=False }
        A.BNE -> OpInfo{ oPrec=4, oAscL=False, oAscR=False }

--------------------------------------------------------------------------------
-- Show instances (for Haskell-like string representations).

instance Show Expr where
    showsPrec prec = (++) . showExpr prec

instance Show A.Int where
    showsPrec prec = showsPrec prec . I

instance Show A.Bool where
    showsPrec prec = showsPrec prec . B

instance Show A.IIBi where
    show A.IAdd = " + "
    show A.ISub = " - "
    show A.IMul = " * "
    show A.IDiv = " `quot` "
    show A.IMod = " `rem` "
    show A.IPow = "^"

instance Show A.IIUn where
    show A.INeg = "-"
    show A.IAbs = "abs "

instance Show A.BBBi where
    show A.BAnd = " && "
    show A.BOr  = " || "

instance Show A.BBUn where
    show A.BNot = "not "

instance Show A.BIBi where
    show A.BLT = " < "
    show A.BGT = " > "
    show A.BLE = " <= "
    show A.BGE = " >= "
    show A.BEq = " == "
    show A.BNE = " /= "

showExpr :: Prec -> Expr -> String
showExpr pPrec expr = case expr of
    _ | prec <= pPrec -> "(" ++ showExpr 0 expr ++ ")"
    I int -> case int of
        A.ICon c      -> show c
        A.IVar v      -> v
        A.IIBi op x y -> showL(I x) ++ show op ++ showR(I y)
        A.IIUn op x   -> show op ++ showR(I x)
        A.IIf b x y   -> "if "++ showL(B b) ++
                         " then "++ showExpr prec (I x) ++ " else "++ showR(I y)
    B bool -> case bool of
        A.True                 -> "True"
        A.False                -> "False"
        A.BBBi op p q          -> showL(B p) ++ show op ++ showR(B q)
        A.BBUn op p            -> show op ++ showR(B p)
        A.BInt (A.BIBi op x y) -> showL(I x) ++ show op ++ showR(I y)
  where
    OpInfo{ oPrec=prec, oAscL=ascL, oAscR=ascR } = opInfo Haskell expr
    showL = showExpr $ if ascL then prec-1 else prec
    showR = showExpr $ if ascR then prec-1 else prec


--------------------------------------------------------------------------------
-- General values conditional on Abstract.Bool, using an "if-then-else"
-- structure, with Monad/Applicative/Functor instances analogous to those of [].
data Cond a
  = Cond{ cIf::A.Bool, cThen::a, cElse::Cond a }
  | CDef a

instance Monad Cond where
    Cond{ cIf=b, cThen=x, cElse=my } >>= f = case f x of
      Cond{ cIf=b', cThen=x', cElse=my' } -> do 
        -- if b     then (f x)                   else (f y) =
        -- if b     then (if b' then x' else y') else (f y) =
        -- if b&&b' then x'                      else (if b then y' else (f y))
        let my'' = my' >>= \y' -> Cond{ cIf=b, cThen=y', cElse=my >>= f }
        Cond{ cIf=b A.&& b', cThen=x', cElse=my'' }
      CDef x' -> do
        -- if b then (f x) else (f y) =
        -- if b then x'    else (f y)
        Cond{ cIf=b, cThen=x', cElse=my>>=f }
    CDef x >>= f = f x
    return = CDef

instance Applicative Cond where
    pure      = return
    mf <*> mx = mf >>= \f -> f <$> mx

instance Functor Cond where
    fmap f mx = mx >>= return . f

--------------------------------------------------------------------------------
-- Conversion of structures with nested conditionals to conditionals in terms of
-- definite versions of those structures, i.e. with no nested conditionals.

-- Definite version of Abstract.Int.
data DefInt
  = DICon Integer
  | DIVar String
  | DIIBi A.IIBi DefInt DefInt
  | DIIUn A.IIUn DefInt

-- Definite version of Abstract.BoolInt.
data DefBoolInt
  = DBIBi A.BIBi DefInt DefInt

-- Definite version of Abstract.Bool.
data DefBool
  = DTrue
  | DFalse
  | DBBBi A.BBBi DefBool DefBool
  | DBBUn A.BBUn DefBool
  | DBInt DefBoolInt

-- Move conditionals in Abstract.Int to the top level.
intToCond :: A.Int -> Cond DefInt
intToCond int = case int of
    A.ICon c      -> DICon <$> pure c
    A.IVar v      -> DIVar <$> pure v
    A.IIBi op x y -> DIIBi op <$> intToCond x <*> intToCond y
    A.IIUn op x   -> DIIUn op <$> intToCond x
    A.IIf b x y   -> Cond b `flip` intToCond y =<< intToCond x

-- Move conditionals in Abstract.BoolInt to the top level.
boolIntToCond :: A.BoolInt -> Cond DefBoolInt
boolIntToCond bool = case bool of
    A.BIBi op x y -> DBIBi op <$> intToCond x <*> intToCond y

-- Convert a conditional BoolInt into a pure boolean structure.
condToBool :: Cond DefBoolInt -> DefBool
condToBool Cond{ cIf=b, cThen=x, cElse=y }
  = DBBBi A.BOr (DBBBi A.BAnd b' $ DBInt x) (DBBBi A.BAnd b'' $ condToBool y)
  where
    b'  = boolToDef b
    b'' = DBBUn A.BNot b'
condToBool (CDef b) = DBInt b

-- Convert conditionals in Abstract.Bool into pure boolean structure.
boolToDef :: A.Bool -> DefBool
boolToDef bool = case bool of
    A.True         -> DTrue
    A.False        -> DFalse
    A.BBBi op b b' -> DBBBi op (boolToDef b) (boolToDef b')
    A.BBUn op b    -> DBBUn op (boolToDef b)
    A.BInt b       -> condToBool . boolIntToCond $ b

--------------------------------------------------------------------------------
-- Conversion to classical propositions.

boolToProp :: A.Bool -> Logic.Prop DefBoolInt
boolToProp = defBoolToProp . boolToDef

defBoolToProp :: DefBool -> Logic.Prop DefBoolInt
defBoolToProp bool = case bool of
    DTrue             -> Logic.PTrue
    DFalse            -> Logic.PFalse
    DBBBi A.BAnd b b' -> Logic.PAnd (defBoolToProp b) (defBoolToProp b')
    DBBBi A.BOr  b b' -> Logic.POr  (defBoolToProp b) (defBoolToProp b')
    DBBUn A.BNot b    -> Logic.PNot (defBoolToProp b)
    DBInt bi          -> Logic.PAtom bi

--------------------------------------------------------------------------------
-- Conversion to ASP syntax.

boolToPropASP :: A.Bool -> Logic.Prop ASP.Literal
boolToPropASP = fmap boolIntToASP . boolToProp

boolIntToASP :: DefBoolInt -> ASP.Literal
boolIntToASP bool = case bool of
    DBIBi op x y -> ASP.LCompare $ ASP.CBiOp (bi op) (intToASP x) (intToASP y)
  where
    bi op = case op of {
        A.BLT->ASP.CLT; A.BLE->ASP.CLT; A.BEq->ASP.CEq;
        A.BGT->ASP.CGT; A.BGE->ASP.CGE; A.BNE->ASP.CNE }

intToASP :: DefInt -> ASP.Expr
intToASP int = case int of
    DICon c         -> ASP.ETerm $ ASP.TInt c
    DIVar v         -> ASP.ETerm $ ASP.TVar (ASP.Variable v)
    DIIBi op x y    -> ASP.EBiOp (bi op) (intToASP x) (intToASP y)
    DIIUn op x      -> ASP.EUnOp (un op) (intToASP x)
  where
    bi op = case op of {
        A.IAdd->ASP.EAdd; A.IMul->ASP.EMul; A.IMod->ASP.EMod;
        A.ISub->ASP.ESub; A.IDiv->ASP.EDiv; A.IPow->ASP.EPow }
    un op = case op of {
        A.INeg->ASP.ENeg; A.IAbs->ASP.EAbs }

--------------------------------------------------------------------------------
-- Conversion from strings in Haskell syntax.

type HsExpr  = String
type HsVar   = String
data HsInput = HsScalar A.Int | HsArray [A.Int]

haskellToBool' :: HsExpr -> [(HsVar, HsInput)] -> IO A.Bool
haskellToBool' expr vars
  = haskellToBool expr vars >>= \result -> case result of
        Left err   -> putStrLn err >> fail "haskellToBool' failed."
        Right bool -> return bool

haskellToBool :: HsExpr -> [(HsVar, HsInput)] -> IO (Either String A.Bool)
haskellToBool expr vars = do
    result <- runInterpreter $ do
        loadModules ["Abstract.Main"]
        setImports  ["Abstract.Main"]
        set [languageExtensions := [RebindableSyntax]]
        interpret ("\\"++ sArgs ++" "++ aArgs ++" -> "++ expr) infer
    return $ case result of
        Left (UnknownError e) -> Left $ "Unknown error: " ++ e
        Left (NotAllowed   e) -> Left $ "Not allowed: "   ++ e
        Left (GhcException e) -> Left $ "GHC exception: " ++ e
        Left (WontCompile es) -> Left $ unlines [e | GhcError e <- es]
        Right lambda            -> Right $ lambda sIns aIns
  where
    (sVars,sIns) = unzip [(v,i) | (v, HsScalar i) <- vars]
    (aVars,aIns) = unzip [(v,i) | (v, HsArray  i) <- vars]
    sArgs = "["++ intercalate "," sVars ++"]"
    aArgs = "["++ intercalate "," aVars ++"]" 
