module While where

import Control.Monad
import Control.Applicative
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
  = ECon Integer -- ..., -1, 0, 1, 2, ...
  | EVar Var     -- x
  | EAdd Var Var -- x + y
  | ESub Var Var -- x - y
  | EMul Var Var -- x * y
  | EDiv Var Var -- x / y
  deriving Show

data Guard
  = GLT Var Var -- x <  y
  | GLE Var Var -- x <= y
  | GGT Var Var -- x >  y
  | GGE Var Var -- x >= y
  | GEQ Var Var -- x == y
  | GNE Var Var -- x != y
  | GNeg Guard  -- !(...)
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
    EMul (Name x) (Name y) -> x ++ " * " ++ y
    EDiv (Name x) (Name y) -> x ++ " / " ++ y

readExpr :: Term -> Maybe Expr
readExpr term
  = readBinary (Name "add") EAdd term <|>
    readBinary (Name "sub") ESub term <|>
    readBinary (Name "mul") EMul term <|>
    readBinary (Name "div") EDiv term <|>
    readLeafExpr term

readLeafExpr :: Term -> Maybe Expr
readLeafExpr term = case term of
    TFun (Name "con") [TInt int]    -> return (ECon int)
    TFun (Name "var") [TFun var []] -> return (EVar var)
    _                               -> Nothing
-------------------------------------------------------------------------------
showGuard :: Guard -> String
showGuard guard = case guard of
    (GLT (Name x) (Name y)) -> x ++ " < " ++ y
    (GGT (Name x) (Name y)) -> x ++ " > " ++ y
    (GLE (Name x) (Name y)) -> x ++ " <= " ++ y
    (GGE (Name x) (Name y)) -> x ++ " >= " ++ y
    (GEQ (Name x) (Name y)) -> x ++ " == " ++ y
    (GNE (Name x) (Name y)) -> x ++ " != " ++ y
    (GNeg guard')           -> "!(" ++ showGuard guard' ++ ")"

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
    TFun (Name "neg") [guardTerm] <- return term
    guard <- readGuard guardTerm
    return (GNeg guard)

-------------------------------------------------------------------------------
readBinary :: Name -> (Var -> Var -> a) -> Term -> Maybe a
readBinary name con term = do
    TFun name' [TFun lVar [], TFun rVar []] <- return term
    guard (name == name')
    return (con lVar rVar)
