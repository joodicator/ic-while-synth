{-# OPTIONS_GHC -fno-warn-orphans #-}

module Abstract.Util() where

--------------------------------------------------------------------------------
-- Utilities for the module Abstract, that we wish to define in the presence
-- of the standard Prelude bindings.

import qualified Abstract.Base as A
import qualified ASP

--------------------------------------------------------------------------------
-- Information about operators in various contexts.

type Prec = Int

data OpInfo = OpInfo{  
    oPrec :: Prec,     -- Precedence
    oAscL :: Bool,     -- Left-associativity
    oAscR :: Bool }    -- Right-associativity

data Context = Haskell

data Expr = I A.Int | B A.Bool

class Operator a where
    info :: Context -> a -> OpInfo

instance Operator Expr where
    info cxt (I int)  = info cxt int
    info cxt (B bool) = info cxt bool

instance Operator A.Int where
    info Haskell int = case int of
        A.ICon _      -> OpInfo{ oPrec=10, oAscL=False, oAscR=False }
        A.IVar _      -> OpInfo{ oPrec=10, oAscL=False, oAscR=False }
        A.IIf _ _ _   -> OpInfo{ oPrec=1, oAscL=False, oAscR=True }
        A.IIBi op _ _ -> info Haskell op
        A.IIUn op _   -> info Haskell op

instance Operator A.IIBi where
    info Haskell op = case op of
        A.IAdd -> OpInfo{ oPrec=5, oAscL=True, oAscR=True }
        A.ISub -> OpInfo{ oPrec=5, oAscL=True, oAscR=False }
        A.IMul -> OpInfo{ oPrec=6, oAscL=True, oAscR=True }
        A.IDiv -> OpInfo{ oPrec=6, oAscL=True, oAscR=False }
        A.IMod -> OpInfo{ oPrec=6, oAscL=True, oAscR=False }
        A.IPow -> OpInfo{ oPrec=7, oAscL=False, oAscR=True }

instance Operator A.IIUn where
    info Haskell op = case op of
        A.INeg -> OpInfo{ oPrec=5, oAscL=False, oAscR=False }
        A.IAbs -> OpInfo{ oPrec=9, oAscL=False, oAscR=False }
        A.ISgn -> OpInfo{ oPrec=9, oAscL=False, oAscR=False }

instance Operator A.Bool where
    info Haskell bool = case bool of
        A.True        -> OpInfo{ oPrec=10, oAscL=False, oAscR=False }
        A.False       -> OpInfo{ oPrec=10, oAscL=False, oAscR=False }
        A.BBBi op _ _ -> info Haskell op
        A.BBUn op _   -> info Haskell op
        A.BIBi op _ _ -> info Haskell op

instance Operator A.BBBi where
    info Haskell op = case op of
        A.BAnd -> OpInfo{ oPrec=3, oAscL=True, oAscR=True }
        A.BOr  -> OpInfo{ oPrec=2, oAscL=True, oAscR=True }

instance Operator A.BBUn where
    info Haskell op = case op of
        A.BNot -> OpInfo{ oPrec=9, oAscL=False, oAscR=False }

instance Operator A.BIBi where
    info Haskell op = case op of
        A.BLT -> OpInfo{ oPrec=4, oAscL=False, oAscR=False }
        A.BGT -> OpInfo{ oPrec=4, oAscL=False, oAscR=False }
        A.BLE -> OpInfo{ oPrec=4, oAscL=False, oAscR=False }
        A.BGE -> OpInfo{ oPrec=4, oAscL=False, oAscR=False }
        A.BEq -> OpInfo{ oPrec=4, oAscL=False, oAscR=False }
        A.BNE -> OpInfo{ oPrec=4, oAscL=False, oAscR=False }

--------------------------------------------------------------------------------
-- Show instances (for Haskell-like string representations).

instance Show Expr where
    show = showExpr 0

instance Show A.Int where
    show = show . I

instance Show A.Bool where
    show = show . B

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
    show A.ISgn = "signum "

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
        A.True        -> "True"
        A.False       -> "False"
        A.BBBi op p q -> showL(B p) ++ show op ++ showR(B q)
        A.BBUn op p   -> show op ++ showR(B p)
        A.BIBi op x y -> showL(I x) ++ show op ++ showR(I y)
  where
    OpInfo{ oPrec=prec, oAscL=ascL, oAscR=ascR } = info Haskell expr
    showL = showExpr $ if ascL then prec-1 else prec
    showR = showExpr $ if ascR then prec-1 else prec
