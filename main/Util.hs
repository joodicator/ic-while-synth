{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances,
             TypeFamilies #-}

module Util where

import Control.Applicative
import Data.Maybe
import Data.Monoid
import Data.Functor.Identity
import Data.MonoTraversable

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

--------------------------------------------------------------------------------
-- Class of types that allow logical negation.

class Negation a where
    negation :: a -> a

--------------------------------------------------------------------------------
-- Default implementation of Mono{Traversable,Foldable,Functor}.

class MonoTraversableD mono where
    otraverseD :: Applicative f =>
        (Element mono -> f (Element mono)) -> mono -> f mono

instance MonoTraversableD mono => MonoTraversable mono where
    otraverse = otraverseD
    omapM act = unwrapMonad . otraverse (WrapMonad . act)

instance MonoTraversableD mono => MonoFoldable mono where
    ofoldMap f    = getConst . otraverseD (Const . f)
    ofoldr  f z t = appEndo (ofoldMap (Endo . f) t) z
    ofoldl' f     = ofoldlDefault $ \l r -> let l' = f l r in l' `seq` l'

    ofoldr1Ex f t
      = fromMaybe (error "ofoldr1Ex: empty") (ofoldr mf Nothing t)
      where mf l Nothing  = Just l
            mf l (Just r) = Just (f l r)

    ofoldl1Ex' f t
      = fromMaybe (error "ofoldlEx': empty") (ofoldlDefault mf Nothing t)
      where mf Nothing  r = Just r
            mf (Just l) r = l' `seq` Just l' where l' = f l r

ofoldlDefault :: MonoFoldable mono => (a -> Element mono -> a) -> a -> mono -> a
ofoldlDefault f z t = appEndo (getDual (ofoldMap (Dual . Endo . flip f) t)) z

instance MonoTraversableD mono => MonoFunctor mono where
    omap f = runIdentity . otraverse (Identity . f)

