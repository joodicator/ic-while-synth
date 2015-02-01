module While where

import Control.Monad
import Data.List
import Clingo

type LineInstr = (Integer, Instr)
type Len = Integer
type Var = Name

data Instr
  = ISet Var Expr
  | IIf Guard Len
  | IWhile Guard Len
  deriving Show

data Expr
  = ECon Integer
  | EVar Var
  | EAdd Var Var
  | ESub Var Var
  deriving Show

data Guard
  = GLT Var Var
  | GGT Var Var
  | GNeg Guard
  deriving Show

--------------------------------------------------------------------------------
showProgram :: [LineInstr] -> [String]
showProgram lines
  = showProgram' "" (sortBy (compare `on` fst) lines)
  where on f g x y = f (g x) (g y)

showProgram' :: String -> [LineInstr] -> [String]
showProgram' indent ((lineNum,instr):lines)
  = [lineNumStr ++ indent ++ head] ++
    showProgram' ("    " ++ indent) (genericTake bodyLength lines) ++
    showProgram'            indent  (genericDrop bodyLength lines)
  where
    (head, bodyLength) = case instr of
        ISet   (Name x) expr ->
            (x ++ " = " ++ showExpr expr, 0)
        IIf    guard len ->
            ("if (" ++ showGuard guard ++ "):", len)
        IWhile guard len ->
            ("while (" ++ showGuard guard ++ "):", len)
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
        return (ISet var expr)
    TFun (Name "if") [tBool, TInt len] -> do
        bool <- readGuard tBool
        return (IIf bool len)
    TFun (Name "while") [tBool, TInt len] -> do
        bool <- readGuard tBool
        return (IWhile bool len)
    _ -> do
        Nothing

--------------------------------------------------------------------------------
showExpr :: Expr -> String
showExpr expr = case expr of
    ECon n                 -> show n
    EVar (Name x)          -> x
    EAdd (Name x) (Name y) -> x ++ " + " ++ y
    ESub (Name x) (Name y) -> x ++ " - " ++ y

readExpr :: Term -> Maybe Expr
readExpr term = case term of
    TFun (Name "con") [TInt int]                  -> return (ECon int)
    TFun (Name "var") [TFun var []]               -> return (EVar var)
    TFun (Name "add") [TFun var [], TFun var' []] -> return (EAdd var var')
    TFun (Name "sub") [TFun var [], TFun var' []] -> return (ESub var var')
    _                                             -> Nothing

--------------------------------------------------------------------------------
showGuard :: Guard -> String
showGuard guard = case guard of
    (GLT (Name x) (Name y))         -> x ++ " < " ++ y
    (GGT (Name x) (Name y))         -> x ++ " > " ++ y
    (GNeg (GLT (Name x) (Name y)))  -> x ++ " >= " ++ y
    (GNeg (GGT (Name x) (Name y)))  -> x ++ " <= " ++ y

readGuard :: Term -> Maybe Guard
readGuard term = case term of
    TFun (Name "lt") [TFun var [], TFun var' []] -> do
        return (GLT var var')
    TFun (Name "gt") [TFun var [], TFun var' []] -> do
        return (GGT var var')
    TFun (Name "neg") [tBool] -> do
        bool <- readGuard tBool
        return (GNeg bool)
    _ -> do
        Nothing 
