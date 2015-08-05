{-# LANGUAGE OverloadedStrings #-}

module While where

import Control.Monad
import Control.Applicative
import Data.List

import Clingo
import Util

type LineNumber = Integer
type LineInstr = (LineNumber, Instr)
type Len = Integer
type Var = Name
type Arr = Name

data Instr
  = ISet Target Expr
  | IIf Guard Len
  | IWhile Guard Len
  | IEndWhile
  deriving Show

data Target
  = TVar Var
  | TArr Arr Expr
  deriving Show

data Expr
  = ECon Integer   -- ... -2, -1, 0, 1, 2 ...
  | EVar Var       -- x
  | EArr Arr Expr  -- xs[E]
  | EAdd Expr Expr -- E1 + E2
  | ESub Expr Expr -- E1 - E2
  | EMul Expr Expr -- E1 * E2
  | EDiv Expr Expr -- E1 / E2
  | EMod Expr Expr -- E1 % E2
  deriving Show

data Guard
  = GLT Expr Expr -- E1 <  E1
  | GLE Expr Expr -- E1 <= E2
  | GGT Expr Expr -- E1 >  E2
  | GGE Expr Expr -- E1 >= E2
  | GEQ Expr Expr -- E1 == E2
  | GNE Expr Expr -- E1 != E2
  | GNeg Guard  -- !( ... )
  deriving Show

--------------------------------------------------------------------------------
showProgram :: [LineInstr] -> [String]
showProgram pLines
  = showProgram' "" (sortBy (compare `on` fst) pLines)
  where on f g x y = f (g x) (g y)

showProgram' :: String -> [LineInstr] -> [String]
showProgram' indent ((lineNum,instr):pLines)
  = [lineNumStr ++ indent ++ pHead] ++
    showProgram' ("    " ++ indent) (genericTake bodyLength pLines) ++
    showProgram'            indent  (genericDrop bodyLength pLines)
  where
    (pHead, bodyLength) = case instr of
        ISet (TVar (Name x)) expr ->
            (x ++ " = " ++ showExpr expr, 0)
        ISet (TArr (Name xs) lExpr) rExpr ->
            (xs ++ "[" ++ showExpr lExpr ++ "] = " ++ showExpr rExpr , 0)
        IIf iGuard len ->
            ("if (" ++ showGuard iGuard ++ "):", len)
        IWhile wGuard len ->
            ("while (" ++ showGuard wGuard ++ "):", len)
        IEndWhile ->
            ("end_while", 0)
    lineNumStr
      = reverse $ take 6 $ (reverse (show lineNum ++ ". ")) ++ repeat ' '

showProgram' _ _ = []

--------------------------------------------------------------------------------
readLineInstr :: Fact -> Maybe LineInstr
readLineInstr fact = case fact of
    Fact (Name "line_instr") [TInt line, tInstr] -> do
        instr <- readInstr tInstr
        return (line, instr)
    _ -> do
        Nothing

--------------------------------------------------------------------------------
readInstr :: Term -> Maybe Instr
readInstr term = case term of
    TFun (Name "set") [TFun var [], tExpr] -> do
        expr <- readExpr tExpr
        return (ISet (TVar var) expr)
    TFun (Name "set") [TFun "array" [TFun arr [], tlExpr], trExpr] -> do
        lExpr <- readExpr tlExpr
        rExpr <- readExpr trExpr
        return (ISet (TArr arr lExpr) rExpr)
    TFun (Name "if") [tBool, TInt len] -> do
        bool <- readGuard tBool
        return (IIf bool len)
    TFun (Name "while") [tBool, TInt len] -> do
        bool <- readGuard tBool
        return (IWhile bool len)
    TFun (Name "end_while") [] -> do
        return IEndWhile
    _ -> do
        Nothing

--------------------------------------------------------------------------------
showExpr :: Expr -> String
showExpr = showExprPrec 0

showExprPrec :: Int -> Expr -> String
showExprPrec p expr
  | p > q     = "(" ++ str ++ ")"
  | otherwise = str
  where
    (q, str) = case expr of
        ECon n           -> (1, show n)
        EVar (Name x)    -> (1, x)
        EArr (Name xs) i -> (1, xs++"["++ showExpr i ++"]")
        EAdd e1 e2       -> (0, showExprPrec q e1 ++ " + " ++ showExprPrec q e2)
        ESub e1 e2       -> (0, showExprPrec q e1 ++ " - " ++ showExprPrec q e2)
        EMul e1 e2       -> (0, showExprPrec q e1 ++ " * " ++ showExprPrec q e2)
        EDiv e1 e2       -> (0, showExprPrec q e1 ++ " / " ++ showExprPrec q e2)
        EMod e1 e2       -> (0, showExprPrec q e1 ++ " % " ++ showExprPrec q e2)

exprToTerm :: Expr -> Term
exprToTerm expr = case expr of
    ECon n      -> TFun "con" [TInt n]
    EVar x      -> TFun "var" [TFun x []]
    EArr xs i   -> TFun "array" [TFun xs [], exprToTerm i]
    EAdd e1 e2  -> TFun "sub" [exprToTerm e1, exprToTerm e2]
    ESub e1 e2  -> TFun "add" [exprToTerm e1, exprToTerm e2]
    EMul e1 e2  -> TFun "mul" [exprToTerm e1, exprToTerm e2]
    EDiv e1 e2  -> TFun "div" [exprToTerm e1, exprToTerm e2]
    EMod e1 e2  -> TFun "mod" [exprToTerm e1, exprToTerm e2]

readExpr :: Term -> Maybe Expr
readExpr term
  = readBinary (Name "add") EAdd term <|>
    readBinary (Name "sub") ESub term <|>
    readBinary (Name "mul") EMul term <|>
    readBinary (Name "div") EDiv term <|>
    readBinary (Name "mod") EMod term <|>
    readLeafExpr term

readLeafExpr :: Term -> Maybe Expr
readLeafExpr term = case term of
    TFun (Name "con") [TInt int] -> do
        return (ECon int)
    TFun (Name "var") [TFun var []] -> do
        return (EVar var)
    TFun (Name "array") [TFun arr [], tExpr] -> do
        expr <- readExpr tExpr
        return (EArr arr expr)
    _ -> Nothing

-------------------------------------------------------------------------------
showGuard :: Guard -> String
showGuard sGuard = case sGuard of
    GLT e1 e2   -> showExpr e1 ++ " < "  ++ showExpr e2
    GGT e1 e2   -> showExpr e1 ++ " > "  ++ showExpr e2
    GLE e1 e2   -> showExpr e1 ++ " <= " ++ showExpr e2
    GGE e1 e2   -> showExpr e1 ++ " >= " ++ showExpr e2
    GEQ e1 e2   -> showExpr e1 ++ " == " ++ showExpr e2
    GNE e1 e2   -> showExpr e1 ++ " != " ++ showExpr e2
    GNeg nGuard -> "!(" ++ showGuard nGuard ++ ")"

guardToTerm :: Guard -> Term
guardToTerm tGuard = case tGuard of
    GLT e1 e2   -> TFun "lt" [exprToTerm e1, exprToTerm e2]
    GGT e1 e2   -> TFun "gt" [exprToTerm e1, exprToTerm e2]
    GLE e1 e2   -> TFun "le" [exprToTerm e1, exprToTerm e2]
    GGE e1 e2   -> TFun "ge" [exprToTerm e1, exprToTerm e2]
    GEQ e1 e2   -> TFun "eq" [exprToTerm e1, exprToTerm e2]
    GNE e1 e2   -> TFun "ne" [exprToTerm e1, exprToTerm e2]
    GNeg nGuard -> TFun "not" [guardToTerm nGuard]

readGuard :: Term -> Maybe Guard
readGuard term
  = readBinary (Name "lt") GLT term <|>
    readBinary (Name "gt") GGT term <|>
    readBinary (Name "le") GLE term <|>
    readBinary (Name "ge") GGE term <|>
    readBinary (Name "eq") GEQ term <|>
    readBinary (Name "ne") GNE term <|>
    readNegation term

readNegation :: Term -> Maybe Guard
readNegation term = do
    TFun (Name "not") [guardTerm] <- return term
    pGuard <- readGuard guardTerm
    return (GNeg pGuard)

-- Negate a guard without increasing the tree depth.
instance Negation Guard where
    negation pGuard = case pGuard of
        GLT e1 e2   -> GGE e1 e2
        GGT e1 e2   -> GLE e1 e2
        GLE e1 e2   -> GGT e1 e2
        GGE e1 e2   -> GLE e1 e2
        GEQ e1 e2   -> GNE e1 e2
        GNE e1 e2   -> GEQ e1 e2
        GNeg nGuard -> nGuard

-------------------------------------------------------------------------------
readBinary :: Name -> (Expr -> Expr -> a) -> Term -> Maybe a
readBinary name con term = do
    TFun name' [t1, t2] <- return term
    guard (name == name')
    e1 <- readExpr t1
    e2 <- readExpr t2
    return (con e1 e2)
