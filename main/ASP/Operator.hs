{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}

module ASP.Operator where

import ASP.Base
import Util

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
