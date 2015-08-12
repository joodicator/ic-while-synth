{-# OPTIONS_GHC -fno-warn-orphans #-}

module ASP.Show where

import ASP.Base
import ASP.Operator

import Util

import Data.List
import Data.Char

--------------------------------------------------------------------------------
-- Show instances for ASP-syntax string representations.

instance Show Predicate where
    show (Predicate p)
      | any isLower (take 1 p) = p
      | otherwise = error $ "\""++ p ++"\" is not a valid ASP predicate name."

instance Show Variable where
    show (Variable v)
      | any isUpper (take 1 v)  = v
      | any (== '_') (take 1 v) = v
      | otherwise = error $ "\""++ v ++"\" is not a valid ASP variable."

instance Show Constant where
    show (Constant c)
      | any isLower (take 1 c) = c
      | otherwise = error $ "\""++ c ++ "\" is not a valid ASP constant."

instance Show Function where
    show (Function f)
      | all isLower (take 1 f) = f
      | otherwise = error $ "\""++ f ++ "\" is not a valid ASP function symbol."

instance Show LuaFunc where
    show (LuaFunc f)
      | not (null f) = "@" ++ f
      | otherwise = error $ "\""++ f ++ "\" is not a valid Lua function name."

instance Show Rule where
    show (Head [] :- Body []) = ":-."
    show (Head [] :- rBody)   = ":- " ++ show rBody ++ "."
    show (rHead   :- Body []) = show rHead ++ "."
    show (rHead   :- rBody)   = show rHead ++ " :- " ++ show rBody ++ "."

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
showArgs xs
  | null xs   = ""
  | otherwise = "("++ intercalate "," (map show xs) ++")"

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
        TStr s    -> show s
        TFun f xs -> show f ++ showArgs xs
        TLua f xs -> show f ++ showArgs xs
