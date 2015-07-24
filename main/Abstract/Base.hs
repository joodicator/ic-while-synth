{-# LANGUAGE NoImplicitPrelude, RebindableSyntax, OverloadedStrings,
             FlexibleInstances, UndecidableInstances,
             NoMonomorphismRestriction, ExtendedDefaultRules #-}

--------------------------------------------------------------------------------
-- Replacements for various Prelude types and classes allowing abstract
-- expression trees to be constructed.
--
-- For general use, Abstract.Main should be imported instead of this module.

module Abstract.Base(
    Bool(..), IIBi(..), IIUn(..), BBBi(..), BBUn(..), BIBi(..),
    Int(..), (^), even, odd,
    Eq(..), Ord(..), ifThenElse,
    length, and, or, any, all,
    module Prelude,
    module Data.List,
    module Data.String
) where

import Prelude hiding (
    Bool(..), (&&), (||), not,
    Int, (^), even, odd,
    Eq(..), Ord(..),
    length, and, or, any, all)
import qualified Prelude

import Data.List hiding (length, and, or, any, all)

import Data.Char (isDigit)
import Data.String (IsString(..))
import Data.Maybe (listToMaybe)
import Control.Monad (guard)

default (Bool, Int)

--------------------------------------------------------------------------------
-- Generalised booleans.
data Bool
  = True                -- Constant True
  | False               -- Constant False
  | BBBi BBBi Bool Bool -- Boolean-valued binary operation on booleans
  | BBUn BBUn Bool      -- Boolean-valued unary operation on booleans
  | BIBi BIBi Int Int   -- Boolean-valued binary operation on integers

data BBBi = BAnd | BOr
data BBUn = BNot
data BIBi = BLT | BGT | BLE | BGE | BEq | BNE

--------------------------------------------------------------------------------
-- Generalised integers.
data Int
  = ICon Integer        -- Integer constant
  | IVar String         -- Integer variable
  | IIBi IIBi Int Int   -- Integer-valued binary operation on integers
  | IIUn IIUn Int       -- Integer-valued unary operation on integers
  | IIf Bool Int Int    -- Conditional integer value

data IIBi = IAdd | ISub | IMul | IDiv | IMod | IPow
data IIUn = INeg | IAbs | ISgn

--------------------------------------------------------------------------------
-- Generalised Bool operations.
class Boolean b where
    infixr 3 &&
    infixr 2 ||
    true :: b
    false :: b
    (&&) :: b -> b -> b   
    (||) :: b -> b -> b
    not :: b -> b
    fromAbstractBool :: Bool -> b
    toAbstractBool   :: b -> Bool

fromBoolean :: (Boolean b, Boolean b') => b -> b'
fromBoolean = fromAbstractBool . toAbstractBool

instance Boolean Bool where
    true  = True
    false = False
    (&&)  = BBBi BAnd
    (||)  = BBBi BOr
    not   = BBUn BNot
    fromAbstractBool = id
    toAbstractBool   = id

instance Boolean Prelude.Bool where
    true  = Prelude.True
    false = Prelude.False
    (&&)  = (Prelude.&&)
    (||)  = (Prelude.||)
    not   = Prelude.not

    fromAbstractBool True  = Prelude.True
    fromAbstractBool False = Prelude.False
    fromAbstractBool _ = error $
        "A non-constant Abstract.Bool was used where Prelude.Bool was expected."
    
    toAbstractBool Prelude.True  = True
    toAbstractBool Prelude.False = False

--------------------------------------------------------------------------------
-- Generalised Int operations.
infixr 8 ^
(^) :: Int -> Int -> Int
(^) = IIBi IPow

even, odd :: Int -> Bool
even x = x `rem` 2 == 0
odd  x = x `rem` 2 /= 0

instance Num Int where
    ICon x + ICon y = ICon $ x + y
    x + y           = IIBi IAdd x y

    ICon x - ICon y = ICon $ x - y
    x - y           = IIBi ISub x y

    ICon x * ICon y = ICon $ x * y
    x * y           = IIBi IMul x y

    negate (ICon x) = ICon $ negate x
    negate x        = IIUn INeg x

    abs (ICon x)    = ICon $ abs x
    abs  x          = IIUn IAbs x

    signum (ICon x) = ICon $ signum x
    signum x        = if x == 0 then 0 else x `quot` abs x

    fromInteger     = ICon

instance IsString Int where
    fromString = IVar

instance Integral Int where
    ICon x `quotRem` ICon y
      = let (q, r) = x `quotRem` y in (ICon q, ICon r)
    x `quotRem` y
      = (IIBi IDiv x y, IIBi IMod x y)

    ICon x `divMod` ICon y
      = let (d, m) = x `divMod` y in (ICon d, ICon m)
    _ `divMod` _ = error $
        "divMod not supported for non-constant Abstract.Int; " ++
        "use quotRem instead."

    toInteger (ICon x) = toInteger x
    toInteger _ = error $
        "toInteger not supported for non-constant Abstract.Int."

instance Real Int where
    toRational (ICon x) = toRational x
    toRational _ = error $
        "toRational not supported for non-constant Abstract.Int."

instance Enum Int where
    toEnum = ICon . toEnum
    fromEnum (ICon x) = fromEnum x
    fromEnum _ = error $
        "fromEnum not supported for non-constant Abstract.Int."

    -- This is an abuse of the Enum class for convenience at the prompt.
    enumFromTo (IVar u) (IVar v) = case mResult of
        Just result -> result
        Nothing -> error $
            "enumFromTo not supported for "++show u++" and "++show v++"."
      where
        mResult = do
            (uName, uNum) <- splitName u
            (vName, vNum) <- splitName v
            guard $ uName Prelude.== vName Prelude.&& vNum Prelude.>= uNum
            return [IVar $ uName ++ show i | i <- [uNum .. vNum]]
        splitName :: String -> Maybe (String, Integer)
        splitName name = do
            (num, []) <- listToMaybe . reads $ reverse h
            return (reverse t, num)
          where (h, t) = span isDigit (reverse name)
    enumFromTo (ICon x) (ICon y)
      = map ICon $ enumFromTo x y
    enumFromTo _ _
      = error "enumFromTo not supported for non-constant Abstract.Int."

instance Prelude.Eq Int where
    x == y = fromBoolean $ x == y
    x /= y = fromBoolean $ x /= y

instance Prelude.Ord Int where
    compare = compare
    (<)  = (<)
    (<=) = (<=)
    (>)  = (>)
    (>=) = (>=)
    max = max
    min = min

--------------------------------------------------------------------------------
-- Generalised branching.
class IfThenElse a where
    ifThenElse :: Bool -> a -> a -> a

instance {-# OVERLAPPABLE #-} IfThenElse a where
    ifThenElse b x y = case fromBoolean b of
        Prelude.True  -> x
        Prelude.False -> y

instance IfThenElse Int where
    ifThenElse = IIf

--------------------------------------------------------------------------------
-- Generalised List operations.
length :: Num i => [a] -> i
length = genericLength

and :: Boolean b => [b] -> b
and = foldr (&&) true

or :: Boolean b => [b] -> b
or = foldr (||) false

all :: Boolean b => (a -> b) -> [a] -> b
all p = foldr ((&&) . p) true

any :: Boolean b => (a -> b) -> [a] -> b
any p = foldr ((||) . p) false

--------------------------------------------------------------------------------
-- Generalised Eq class.
class Eq a where
    infixr 4 ==, /=
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool

instance {-# OVERLAPPABLE #-} Prelude.Eq a => Eq a where
    x == y = fromBoolean $ x Prelude.== y
    x /= y = fromBoolean $ x Prelude./= y

instance Eq Int where
    (ICon x) == (ICon y) = fromBoolean $ x Prelude.== y
    x == y               = fromBoolean $ BIBi BEq x y

    (ICon x) /= (ICon y) = fromBoolean $ x Prelude./= y
    x /= y               = fromBoolean $ BIBi BNE x y

--------------------------------------------------------------------------------
-- Generalised Ord class.
class Ord a where
    infixr 4 <, <=, >, >=
    compare :: a -> a -> Ordering
    (<)  :: Boolean b => a -> a -> b
    (<=) :: Boolean b => a -> a -> b
    (>)  :: Boolean b => a -> a -> b
    (>=) :: Boolean b => a -> a -> b
    max  :: a -> a -> a
    min  :: a -> a -> a

instance {-# OVERLAPPABLE #-} Prelude.Ord a => Ord a where
    compare = Prelude.compare
    x <  y = fromBoolean $ x Prelude.<  y
    x <= y = fromBoolean $ x Prelude.<= y
    x >  y = fromBoolean $ x Prelude.>  y
    x >= y = fromBoolean $ x Prelude.>= y
    max = Prelude.max
    min = Prelude.min

instance Ord Int where
    compare = error $
        "`compare` not supported for Abstract.Int. Use <, ==, etc, instead."

    (ICon x) < (ICon y)   = x < y
    x < y                 = fromBoolean $ BIBi BLT x y

    (ICon x) > (ICon y)   = x > y
    x > y                 = fromBoolean $ BIBi BGT x y

    (ICon x) <= (ICon y)  = x <= y
    x <= y                = fromBoolean $ BIBi BLE x y

    (ICon x) >= (ICon y)  = x >= y
    x >= y                = fromBoolean $ BIBi BGE x y

    max (ICon x) (ICon y) = ICon $ max x y
    max x y               = if x < y then y else x

    min (ICon x) (ICon y) = ICon $ min x y
    min x y               = if x < y then x else y
