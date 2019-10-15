{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
-- |
-- Module       : Data.Vector.NonEmpty
-- Copyright 	: 2019 Emily Pillmore
-- License	: BSD
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: TypeFamilies, MPTC, Rank2Types, DataTypeable, CPP
--
-- A library for non-empty boxed vectors (that is, polymorphic arrays capable of
-- holding any Haskell value). Non-empty vectors come in two flavors:
--
--  * mutable
--
--  * immutable
--
-- and support a rich interface of both list-like operations, and bulk
-- array operations.
--
-- For unboxed non-empty arrays, use "Data.Vector.NonEmpty.Unboxed"
--
-- Credit to Roman Leshchinskiy for the original Vector library
-- upon which this is based.
--
module Data.Vector.NonEmpty
( -- * Boxed non-empty vectors
  NEVector

  -- * Accessors
, length, head, tail, last, (!), (!?)
, unsafeIndex, unsafeLast
  -- * Construction
, cons, snoc, uncons, singleton
  -- * Conversion
, fromNonEmpty, toNonEmpty
, toVector, fromVector
) where


import qualified Prelude
import Prelude hiding ( length, replicate, (++), concat,
                        head, last, init, tail, take, drop, splitAt,
                        reverse, map, concat, concatMap,
                        zipWith, zipWith3, zip, zip3, unzip, unzip3,
                        filter, takeWhile, dropWhile, span, break,
                        elem, notElem, foldl, foldl1, foldr, foldr1,
                        all, any, and, or, sum, product, maximum, minimum,
                        scanl, scanl1, scanr, scanr1,
                        enumFromTo, enumFromThenTo,
                        mapM, mapM_, sequence, sequence_,
                        showsPrec )



import Control.Applicative
import Control.DeepSeq
import Control.Monad (MonadPlus(..))
import Control.Monad.Zip (MonadZip)

import Data.Data (Data)
import Data.Foldable hiding (length)
import Data.Functor.Classes (Eq1, Ord1, Read1, Show1)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Semigroup (Semigroup(..))
import Data.Traversable as Traversable
import Data.Typeable (Typeable)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import Data.Vector.Mutable  ( MVector(..) )

import qualified GHC.Exts as Exts (IsList(..))
import GHC.Generics


newtype NEVector a = NEVector
    { _neVec :: V.Vector a }
    deriving
      ( Eq, Ord, Show, Read
      , Data, Typeable, Generic
      , Foldable
      )

-- ---------------------------------------------------------------------- --
-- Accessors + Indexing

length :: NEVector a -> Int
length = V.length . _neVec
{-# INLINE length #-}

head :: NEVector a -> a
head = V.head . _neVec
{-# INLINE head #-}

tail :: NEVector a -> NEVector a
tail (NEVector as) = NEVector (V.tail as)
{-# INLINE tail #-}

last :: NEVector a -> a
last = V.last . _neVec
{-# INLINE last #-}

(!) :: NEVector a -> Int -> a
(!) (NEVector as) n = as V.! n
{-# INLINE (!) #-}

(!?) :: NEVector a -> Int -> Maybe a
(NEVector as) !? n = as V.!? n
{-# INLINE (!?) #-}

unsafeIndex :: NEVector a -> Int -> a
unsafeIndex (NEVector as) n = V.unsafeIndex as n
{-# INLINE unsafeIndex #-}

unsafeLast :: NEVector a -> a
unsafeLast = V.last . _neVec
{-# INLINE unsafeLast #-}

-- ---------------------------------------------------------------------- --
-- Construction

cons :: a -> NEVector a -> NEVector a
cons a (NEVector as) = NEVector (V.cons a as)
{-# INLINE cons #-}

snoc :: NEVector a -> a -> NEVector a
snoc (NEVector as) a = NEVector (V.snoc as a)
{-# INLINE snoc #-}

uncons :: NEVector a -> (a, Maybe (NEVector a))
uncons (NEVector as) = (V.head as, fromVector as)
{-# INLINE uncons #-}

singleton :: a -> NEVector a
singleton = NEVector . V.singleton
{-# INLINE singleton #-}

-- ---------------------------------------------------------------------- --
-- Conversion

fromNonEmpty :: NonEmpty a -> NEVector a
fromNonEmpty = NEVector . V.fromList . toList
{-# INLINE fromNonEmpty #-}

toNonEmpty :: NEVector a -> NonEmpty a
toNonEmpty = NonEmpty.fromList . toList . _neVec
{-# INLINE toNonEmpty #-}

toVector :: NEVector a -> V.Vector a
toVector = _neVec
{-# INLINE toVector #-}

fromVector :: V.Vector a -> Maybe (NEVector a)
fromVector v = if V.null v then Nothing else Just (NEVector v)
{-# INLINE fromVector #-}
