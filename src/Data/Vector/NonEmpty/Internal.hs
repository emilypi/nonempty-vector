{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
-- |
-- Module      : Data.Vector.NonEmpty.Internal
-- Copyright   : (c) 2019-2026 Emily Pillmore
-- License     : BSD-style
--
-- Maintainer  : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability   : experimental
-- Portability : non-portable
--
-- Internal module exposing the constructors for
-- t'NonEmptyVector' and t'NonEmptyMVector'.
--
-- /Warning/: Since the constructors are exposed here, by using this
-- module, you take on the risks that you break the non-emptiness
-- invariants of the main modules. Use at your own risk.
--
-- @since 0.2.1.0
--
module Data.Vector.NonEmpty.Internal
( -- * Immutable boxed vectors
  NonEmptyVector(..)
  -- * Mutable boxed vectors
, NonEmptyMVector(..)
  -- ** Mutable vector aliases
, NonEmptyIOVector
, NonEmptySTVector
) where


import Control.DeepSeq (NFData)
import Control.Monad.ST
import Control.Monad.Zip (MonadZip)

import Data.Data (Data)
import qualified Data.Foldable as Foldable
import Data.List.NonEmpty (NonEmpty((:|)))
#if MIN_VERSION_base(4,18,0)
import Data.Foldable1 (Foldable1)
import qualified Data.Foldable1 as Foldable1
#endif
import Data.Functor.Classes (Eq1, Ord1, Show1, Read1(..))
import qualified Data.Vector as V
#if __GLASGOW_HASKELL__ < 912
import Data.Typeable (Typeable)
#endif
import Data.Vector.Mutable (MVector)

import qualified Text.Read as Read


-- ---------------------------------------------------------------------- --
-- Non-empty immutable vectors

-- | t'NonEmptyVector' is a thin wrapper around 'Data.Vector.Vector' that
-- witnesses an API requiring non-empty construction,
-- initialization, and generation of non-empty vectors by design.
--
-- A newtype wrapper was chosen so that no new pointer indirection
-- is introduced when working with 'Data.Vector.Vector's, and all performance
-- characteristics inherited from the "Data.Vector" API still apply.
--
-- @since 0.2.1.0
--
newtype NonEmptyVector a = NonEmptyVector
    { _neVec :: V.Vector a
    } deriving
      ( Eq, Ord
      , Eq1, Ord1, Show1
      , Data, NFData
#if __GLASGOW_HASKELL__ < 912
      , Typeable
#endif
      , Functor, Applicative, Monad
      , MonadZip
      , Semigroup
      , Traversable
      )

instance Show a => Show (NonEmptyVector a) where
    show (NonEmptyVector v) = show v

instance Read a => Read (NonEmptyVector a) where
    readPrec = Read.readPrec >>= \case
      [] -> Read.pfail
      as -> return (NonEmptyVector $ V.fromList as)

instance Read1 NonEmptyVector where
    liftReadPrec _ rl = rl >>= \case
      [] -> Read.pfail
      as -> return (NonEmptyVector $ V.fromList as)

instance Foldable NonEmptyVector where
    foldMap f = Foldable.foldMap f . _neVec
    foldr f x = Foldable.foldr f x . _neVec
    foldl f x = Foldable.foldl f x . _neVec
    foldr1 f = Foldable.foldr1 f . _neVec
    foldl1 f = Foldable.foldl1 f . _neVec
    foldr' f x = Foldable.foldr' f x . _neVec
    foldl' f x = Foldable.foldl' f x . _neVec
    toList = Foldable.toList . _neVec
    length = Foldable.length . _neVec
    null = const False
    elem x = Foldable.elem x . _neVec
    maximum = Foldable.maximum . _neVec
    minimum = Foldable.minimum . _neVec
    sum = Foldable.sum . _neVec
    product = Foldable.product . _neVec

#if MIN_VERSION_base(4,18,0)
instance Foldable1 NonEmptyVector where
    foldMap1 f (NonEmptyVector v) = 
        let 
          projected = fmap f v
          h = V.unsafeHead projected -- guaranteed head (nonemptiness)
          t = V.unsafeTail projected -- guaranteed possibly empty tail
        -- Defer to `Vector`'s underlying folding mechanism.
        -- Direction doesn't matter since we're associative anyway.
        in V.foldl (<>) h t
    foldMap1' f (NonEmptyVector v) = 
        let 
          projected = fmap f v
          h = V.unsafeHead projected
          t = V.unsafeTail projected
        in V.foldl' (<>) h t
    toNonEmpty (NonEmptyVector v) = 
        let h = V.unsafeHead v
            t = V.unsafeTail v
        in h :| V.toList t
    maximum = V.maximum . _neVec
    minimum = V.minimum . _neVec
    head = V.unsafeHead . _neVec
    last (NonEmptyVector v) = V.unsafeIndex v (V.length v - 1)
#endif

-- ---------------------------------------------------------------------- --
-- Non-empty mutable vectors

-- | t'NonEmptyMVector' is a thin wrapper around 'MVector' that
-- witnesses an API requiring non-empty construction,
-- initialization, and generation of non-empty vectors by design.
--
-- A newtype wrapper was chosen so that no new pointer indirection
-- is introduced when working with 'MVector's, and all performance
-- characteristics inherited from the 'MVector' API still apply.
--
-- @since 0.2.1.0
--
newtype NonEmptyMVector s a = NonEmptyMVector
    { _nemVec :: MVector s a }
#if __GLASGOW_HASKELL__ < 912
    deriving (Typeable)
#endif

-- | t'NonEmptyMVector' parametrized by 'Control.Monad.Primitive.PrimState' 'IO'.
--
-- @since 0.2.1.0
--
type NonEmptyIOVector = NonEmptyMVector RealWorld

-- | t'NonEmptyMVector' parametrized by @t'Control.Monad.Primitive.PrimState' ('ST' s)@.
--
-- @since 0.2.1.0
--
type NonEmptySTVector s = NonEmptyMVector s
