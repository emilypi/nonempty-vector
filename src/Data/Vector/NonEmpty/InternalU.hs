{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
-- |
-- Module      : Data.Vector.NonEmpty.InternalU
-- Copyright   : (c) 2019-2026 Emily Pillmore, Koz Ross
-- License     : BSD-style
--
-- Maintainer  : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability   : experimental
-- Portability : non-portable
--
-- Internal module exposing the constructors for
-- t'NonEmptyUVector' and t'NonEmptyMUVector'.
--
-- /Warning/: Since the constructors are exposed here, by using this
-- module, you take on the risks that you break the non-emptiness
-- invariants of the main modules. Use at your own risk.
--
-- @since 0.3.0.0
--
module Data.Vector.NonEmpty.InternalU
( -- * Immutable unboxed vectors
  NonEmptyUVector(..)
  -- * Mutable unboxed vectors
, NonEmptyMUVector(..)
  -- ** Mutable unboxed vector aliases
, NonEmptyIOUVector
, NonEmptySTUVector
) where


import Control.DeepSeq (NFData)
import Control.Monad.ST

import Data.Data (Data)
import qualified Data.Vector.Unboxed as V
#if __GLASGOW_HASKELL__ < 912
import Data.Typeable (Typeable)
#endif
import Data.Vector.Unboxed.Mutable (MVector)

import qualified Text.Read as Read


-- ---------------------------------------------------------------------- --
-- Non-empty immutable unboxed vectors

-- | t'NonEmptyUVector' is a thin wrapper around 'Data.Vector.Unboxed.Vector' that
-- witnesses an API requiring non-empty construction,
-- initialization, and generation of non-empty vectors by design.
--
-- A newtype wrapper was chosen so that no new pointer indirection
-- is introduced when working with 'Data.Vector.Unboxed.Vector's, and all performance
-- characteristics inherited from the "Data.Unboxed.Vector" API still apply.
--
-- @since 0.3.0.0
--
newtype NonEmptyUVector a = NonEmptyUVector
    { _neVec :: V.Vector a
    } deriving
      ( Eq, Ord
      , Data, NFData
#if __GLASGOW_HASKELL__ < 912
      , Typeable
#endif
      ) 

deriving via (V.Vector a) instance (Show a, V.Unbox a) => 
  Show (NonEmptyUVector a)

instance (V.Unbox a, Read a) => Read (NonEmptyUVector a) where
    readPrec = Read.readPrec >>= \case
      [] -> Read.pfail
      as -> return (NonEmptyUVector $ V.fromList as)

-- ---------------------------------------------------------------------- --
-- Non-empty unboxed mutable vectors

-- | t'NonEmptyMUVector' is a thin wrapper around 'MVector' that
-- witnesses an API requiring non-empty construction,
-- initialization, and generation of non-empty vectors by design.
--
-- A newtype wrapper was chosen so that no new pointer indirection
-- is introduced when working with 'MVector's, and all performance
-- characteristics inherited from the 'MVector' API still apply.
--
-- @since 0.3.0.0
--
newtype NonEmptyMUVector s a = NonEmptyMUVector
    { _nemVec :: MVector s a }
#if __GLASGOW_HASKELL__ < 912
    deriving (Typeable)
#endif

-- | t'NonEmptyMUVector' parametrized by 'Control.Monad.Primitive.PrimState' 'IO'.
--
-- @since 0.3.0.0
--
type NonEmptyIOUVector = NonEmptyMUVector RealWorld

-- | t'NonEmptyMUVector' parametrized by @t'Control.Monad.Primitive.PrimState' ('ST' s)@.
--
-- @since 0.3.0.0
--
type NonEmptySTUVector s = NonEmptyMUVector s
