{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Data.Functor.Classes (Eq1, Ord1, Show1, Read1(..))
import qualified Data.Vector as V
import Data.Vector.Mutable (MVector)
import Data.Typeable (Typeable)

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
newtype NonEmptyVector a = NonEmptyVector
    { _neVec :: V.Vector a
    } deriving
      ( Eq, Ord
      , Eq1, Ord1, Show1
      , Data, Typeable, NFData
      , Functor, Applicative, Monad
      , MonadZip
      , Semigroup
      )

instance Show a => Show (NonEmptyVector a) where
    show (NonEmptyVector v) = show v

instance Read a => Read (NonEmptyVector a) where
    readPrec = do
      as <- Read.readPrec
      if Foldable.null as
      then Read.pfail
      else return (NonEmptyVector $ V.fromList as)

instance Read1 NonEmptyVector where
#if __GLASGOW_HASKELL__ > 802
    liftReadPrec _ rl = do
      l <- rl
      if Foldable.null l
      then Read.pfail
      else return (NonEmptyVector $ V.fromList l)
#else
    liftReadsPrec _ r _ s = do
      (as, s') <- r s
      if Foldable.null as
      then []
      else return (NonEmptyVector $ V.fromList as, s')
#endif

instance Foldable NonEmptyVector where
    foldMap f = Foldable.foldMap f . _neVec

instance Traversable NonEmptyVector where
    traverse f = fmap NonEmptyVector . traverse f . _neVec

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
newtype NonEmptyMVector s a = NonEmptyMVector
    { _nemVec :: MVector s a }
    deriving (Typeable)

-- | 'NonEmptyMVector' parametrized by 'PrimState'
--
type NonEmptyIOVector = NonEmptyMVector RealWorld

-- | 'NonEmptyMVector' parametrized by 'ST'
--
type NonEmptySTVector s = NonEmptyMVector s
