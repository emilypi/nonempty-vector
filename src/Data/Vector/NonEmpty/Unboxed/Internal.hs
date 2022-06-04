{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Trustworthy #-}
-- |
-- Module      : Data.Vector.NonEmpty.Internal
-- Copyright   : (c) 2019-2020 Emily Pillmore
-- License     : BSD-style
--
-- Maintainer  : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability   : experimental
-- Portability : non-portable
--
-- Internal module exposing the constructors for
-- 'NonEmptyVector' and 'NonEmptyMVector'.
--
-- /Warning/: Since the constructors are exposed here, by using this
-- module, you take on the risks that you break the non-emptiness
-- invariants of the main modules. Use at your own risk.
--
-- @since 0.2.1.0
--
module Data.Vector.NonEmpty.Unboxed.Internal
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

import Data.Data (Data)
import qualified Data.Vector.Unboxed as V
#if __GLASGOW_HASKELL__ < 804
import Data.Semigroup (Semigroup(..))
#endif
import Data.Typeable (Typeable)
import Data.Vector.Unboxed (MVector, Vector)

import qualified Text.Read as Read


-- ---------------------------------------------------------------------- --
-- Non-empty immutable vectors

-- | 'NonEmptyVector' is a thin wrapper around 'Vector' that
-- witnesses an API requiring non-empty construction,
-- initialization, and generation of non-empty vectors by design.
--
-- A newtype wrapper was chosen so that no new pointer indirection
-- is introduced when working with 'Vector's, and all performance
-- characteristics inherited from the 'Vector' API still apply.
--
-- @since 0.2.1.0
--
newtype NonEmptyVector a = NonEmptyVector
    { _neVec :: Vector a
    } deriving
      ( Eq, Ord
      , Data, Typeable, NFData
      , Semigroup
      )

instance (Show a, V.Unbox a) => Show (NonEmptyVector a) where
    show (NonEmptyVector v) = show v

instance (Read a, V.Unbox a) => Read (NonEmptyVector a) where
    readPrec = Read.readPrec >>= \case
      [] -> Read.pfail
      as -> return (NonEmptyVector $ V.fromList as)

-- ---------------------------------------------------------------------- --
-- Non-empty mutable vectors

-- | 'NonEmptyMVector' is a thin wrapper around 'MVector' that
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
    deriving (Typeable)

-- | 'NonEmptyMVector' parametrized by 'PrimState'
--
-- @since 0.2.1.0
--
type NonEmptyIOVector = NonEmptyMVector RealWorld

-- | 'NonEmptyMVector' parametrized by 'ST'
--
-- @since 0.2.1.0
--
type NonEmptySTVector s = NonEmptyMVector s
