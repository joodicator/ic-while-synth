module While where

import Control.Monad
import Control.Applicative
import Data.List
import Clingo

type LineNumber = Integer
type LineInstr = (LineNumber, Instr)
type Len = Integer
type Var = Name

data Instr
  = ISet Var Expr
  | IIf Guard Len
  | IWhile Guard Len
  | IEndWhile
  deriving Show

data Expr
  = ECon Integer   -- ... -2, -1, 0, 1, 2 ...
  | EVar Var       -- x
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
        return (ISet var expr)
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
        ECon n          -> (1, show n)
        EVar (Name x)   -> (1, x)
        EAdd e1 e2      -> (0, showExprPrec q e1 ++ " + " ++ showExprPrec q e2)
        ESub e1 e2      -> (0, showExprPrec q e1 ++ " - " ++ showExprPrec q e2)
        EMul e1 e2      -> (0, showExprPrec q e1 ++ " * " ++ showExprPrec q e2)
        EDiv e1 e2      -> (0, showExprPrec q e1 ++ " / " ++ showExprPrec q e2)
        EMod e1 e2      -> (0, showExprPrec q e1 ++ " % " ++ showExprPrec q e2)

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
    TFun (Name "con") [TInt int]    -> return (ECon int)
    TFun (Name "var") [TFun var []] -> return (EVar var)
    _                               -> Nothing

-------------------------------------------------------------------------------
showGuard :: Guard -> String
showGuard guard = case guard of
    (GLT e1 e2)     -> showExpr e1 ++ " < "  ++ showExpr e2
    (GGT e1 e2)     -> showExpr e1 ++ " > "  ++ showExpr e2
    (GLE e1 e2)     -> showExpr e1 ++ " <= " ++ showExpr e2
    (GGE e1 e2)     -> showExpr e1 ++ " >= " ++ showExpr e2
    (GEQ e1 e2)     -> showExpr e1 ++ " == " ++ showExpr e2
    (GNE e1 e2)     -> showExpr e1 ++ " != " ++ showExpr e2
    (GNeg guard')   -> "!(" ++ showGuard guard' ++ ")"

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
readBinary :: Name -> (Expr -> Expr -> a) -> Term -> Maybe a
readBinary name con term = do
    TFun name' [t1, t2] <- return term
    guard (name == name')
    e1 <- readExpr t1
    e2 <- readExpr t2
    return (con e1 e2)
