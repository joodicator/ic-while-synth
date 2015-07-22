{-# LANGUAGE NoImplicitPrelude, RebindableSyntax, OverloadedStrings #-}

module Abstract where

import qualified Prelude as P
import Prelude (
    Num(..), Integral(..), Show(..),
    String, Maybe(..),
    (.), ($), undefined, otherwise, shows,
    (++), map, foldr, sum, product, reverse)

import Data.String

--------------------------------------------------------------------------------
-- Generalised booleans.
data Bool
  = True                -- Constant True
  | False               -- Constant False
  | BBBi BBBi Bool Bool -- Boolean-valued binary operation on booleans
  | BBUn BBUn Bool      -- Boolean-valued unary operation on booleans
  | BIBi BIBi Int Int   -- Boolean-valued binary operation on integers

data BBBi
  = BAnd | BOr
  deriving P.Eq

data BBUn
  = BNot
  deriving P.Eq

data BIBi
  = BLT | BGT | BLE | BGE | BEq | BNE
  deriving P.Eq

--------------------------------------------------------------------------------
-- Generalised integers.
data Int
  = ICon P.Integer      -- Integer constant
  | IVar P.String       -- Integer variable
  | IIBi IIBi Int Int   -- Integer-valued binary operation on integers
  | IIUn IIUn Int       -- Integer-valued unary operation on integers
  | IIf Bool Int Int    -- Conditional integer value

data IIBi
  = IAdd | ISub | IMul | IDiv | IMod | IPow
  deriving P.Eq
data IIUn
  = INeg | IAbs | ISgn
  deriving P.Eq

--------------------------------------------------------------------------------
-- RebindableSyntax and OverloadedStrings definitions.

infixr 3 &&
infixr 2 ||
(&&), (||) :: Bool -> Bool -> Bool
(&&) = BBBi BAnd
(||) = BBBi BOr

and, or :: [Bool] -> Bool
and = foldr (&&) False 
or  = foldr (||) True

all, any :: (a -> Bool) -> [a] -> Bool
all = (and .) . map
any = (or .) . map

not :: Bool -> Bool
not = BBUn BNot

infixr 4 <, >, <=, >=, ==, /=
(<), (>), (<=), (>=), (==), (/=) :: Int -> Int -> Bool
(<)  = BIBi BLT
(>)  = BIBi BGT
(<=) = BIBi BLE
(>=) = BIBi BGE
(==) = BIBi BEq
(/=) = BIBi BNE

infixr 8 ^
(^) :: Int -> Int -> Int
(^) = IIBi IPow

ifThenElse :: Bool -> Int -> Int -> Int
ifThenElse = IIf

instance Num Int where
    (+)         = IIBi IAdd
    (-)         = IIBi ISub
    (*)         = IIBi IMul
    negate      = IIUn INeg
    abs         = IIUn IAbs
    signum x    = if x == 0 then 0 else x `div` abs x
    fromInteger = ICon

instance IsString Int where
    fromString = IVar

instance Integral Int where
    x `quotRem` y = (IIBi IDiv x y, IIBi IMod x y)
    divMod        = undefined
    toInteger     = undefined

instance P.Real Int where
    toRational = undefined

instance P.Eq Int where
    (==) = undefined

instance P.Ord Int where
    compare = undefined

instance P.Enum Int where
    toEnum   = undefined
    fromEnum = undefined

--------------------------------------------------------------------------------
-- Show instances.

type Prec = P.Int

showInt :: Maybe IIBi -> Prec -> Int -> String
showInt _ _ (ICon c) = show c
showInt _ _ (IVar v) = v
showInt _ p z@(IIUn op x)
  | q P.> p   = show op ++ showInt Nothing q x
  | otherwise = "(" ++ show z ++ ")"
  where q = 6
showInt b p z@(IIBi op x y)
  | q P.> p             = showInt (Just op) q x ++ show op ++
                          showInt (Just op) q y
  | q P.== p P.&& assoc = show z
  | otherwise           = "(" ++ show z ++ ")"
  where
    q = case op of IAdd->3; ISub->4; IMul->5; IDiv->2; IMod->2; IPow->6
    assoc = P.or [op P.== o P.&& b P.== Just o | o <- [IAdd, IMul]]
showInt _ p z@(IIf b x y)
  | q P.> p   = "if " ++ show b ++ " then " ++ showInt Nothing q x ++
                " else " ++ showInt Nothing q y
  | otherwise = "(" ++ show z ++ ")"
  where q = 1

showBool :: Maybe BBBi -> Prec -> Bool -> String
showBool _ _ (BIBi BEq x y) = show x ++ " == " ++ show y

instance Show Int where
    show = showInt Nothing 0

instance Show Bool where
    show = showBool Nothing 0

instance Show IIBi where
    show IAdd = " + "
    show ISub = " - "
    show IMul = " * "
    show IDiv = " `div` "
    show IMod = " `mod` "
    show IPow = "^"

instance Show IIUn where
    show INeg = "-"
    show IAbs = "abs "
    show ISgn = "signum "
