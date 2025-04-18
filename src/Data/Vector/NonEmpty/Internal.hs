{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
-- |
-- Module      : Data.Vector.NonEmpty.Internal
-- Copyright   : (c) 2019-2024 Emily Pillmore
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

#if MIN_VERSION_base(4,18,0)
instance Foldable1 NonEmptyVector where
    foldMap1 f (NonEmptyVector v) =
        let
          h = V.unsafeHead v -- guaranteed head (nonemptiness)
          t = V.unsafeTail v -- guaranteed possibly empty tail
        in go (f h) t -- therefore this is a sound call
      where
        go x xs
          -- when xs is empty, vector is exhausted, return x
          | V.null xs = x
          | otherwise =
          -- if xs is not empty, then there are at least 2 elements in the list. Hence, h and t are sound calls to make.
            let
              h = V.unsafeHead xs
              t = V.unsafeTail xs
            in x <> go (f h) t
#endif

instance Traversable NonEmptyVector where
    traverse f = fmap NonEmptyVector . traverse f . _neVec

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
