{-# LANGUAGE TypeFamilies #-}

module Util where

--------------------------------------------------------------------------------
-- Utilities for dealing with programming language syntax trees.

type Prec = Int

data OpInfo = OpInfo{  
    oPrec :: Prec,     -- Precedence
    oAscL :: Bool,     -- Left-associativity
    oAscR :: Bool }    -- Right-associativity

-- The class of types from which information about a particular operator can
-- be extracted with respect to a particular context.
class Operator a where
    type OpContext a
    opInfo :: OpContext a -> a -> OpInfo
