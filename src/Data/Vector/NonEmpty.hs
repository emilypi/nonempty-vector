{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
  NonEmptyVector

  -- * Accessors
, length

  -- * Indexing
, head, last, (!), (!?)
, unsafeIndex

  -- * Monadic Indexing
, headM, lastM, indexM, unsafeIndexM

  -- * Construction
, cons, snoc, singleton, replicate, generate
, iterateN

  -- * Extracting subvectors (slicing)
, tail, slice, init, take, drop, splitAt
, unsafeSlice, unsafeTake

  -- * Monad Initialization
, replicateM, generateM, iterateNM

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
import Control.Monad.Fail
import Control.Monad.Zip (MonadZip)

import Data.Data (Data)
import Data.Foldable hiding (length)
import Data.Functor.Classes (Eq1, Ord1, Read1, Show1)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Semigroup (Semigroup(..))
import Data.Traversable as Traversable
import Data.Typeable (Typeable)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import Data.Vector.Mutable  ( MVector(..) )

import GHC.Generics


newtype NonEmptyVector a = NonEmptyVector
    { _neVec :: V.Vector a
    } deriving
      ( Eq, Ord, Show, Read
      , Data, Typeable, Generic, NFData
      , Functor, Applicative, Monad
      , MonadFail, MonadZip, Alternative
      )

-- ---------------------------------------------------------------------- --
-- Instances

instance Foldable NonEmptyVector where
    foldMap f = foldMap f . _neVec

instance Traversable NonEmptyVector where
    traverse f = fmap NonEmptyVector . traverse f . _neVec

-- ---------------------------------------------------------------------- --
-- Accessors + Indexing

length :: NonEmptyVector a -> Int
length = V.length . _neVec
{-# INLINE length #-}

-- | /O(1)/ First element.
--
head :: NonEmptyVector a -> a
head = V.unsafeHead . _neVec
{-# INLINE head #-}

-- | /O(1)/ Last element.
--
last :: NonEmptyVector a -> a
last = V.unsafeLast . _neVec
{-# INLINE last #-}

(!) :: NonEmptyVector a -> Int -> a
(!) (NonEmptyVector as) n = as V.! n
{-# INLINE (!) #-}

(!?) :: NonEmptyVector a -> Int -> Maybe a
(NonEmptyVector as) !? n = as V.!? n
{-# INLINE (!?) #-}

unsafeIndex :: NonEmptyVector a -> Int -> a
unsafeIndex (NonEmptyVector as) n = V.unsafeIndex as n
{-# INLINE unsafeIndex #-}

-- ---------------------------------------------------------------------- --
-- Monadic Indexing

indexM :: Monad m => NonEmptyVector a -> Int -> m a
indexM (NonEmptyVector v) n = V.indexM v n
{-# INLINE indexM #-}

headM :: Monad m => NonEmptyVector a -> m a
headM (NonEmptyVector v) = V.unsafeHeadM v
{-# INLINE headM #-}

lastM :: Monad m => NonEmptyVector a -> m a
lastM (NonEmptyVector v) = V.unsafeLastM v
{-# INLINE lastM #-}

unsafeIndexM :: Monad m => NonEmptyVector a -> Int -> m a
unsafeIndexM (NonEmptyVector v) n = V.unsafeIndexM v n
{-# INLINE unsafeIndexM #-}

-- ---------------------------------------------------------------------- --
-- Extracting subvectors (slicing)

tail :: NonEmptyVector a -> Vector a
tail = V.unsafeTail . _neVec
{-# INLINE tail #-}

slice :: Int -> Int -> NonEmptyVector a -> Vector a
slice i n = V.slice i n . _neVec

init :: NonEmptyVector a -> Vector a
init = V.unsafeInit . _neVec

take :: Int -> NonEmptyVector a -> Vector a
take n = V.take n . _neVec

drop :: Int ->  NonEmptyVector a -> Vector a
drop n = V.drop n . _neVec

splitAt :: Int -> NonEmptyVector a -> (Vector a, Vector a)
splitAt n = V.splitAt n . _neVec

unsafeSlice :: Int -> Int -> NonEmptyVector a -> Vector a
unsafeSlice i n = V.unsafeSlice i n . _neVec

unsafeTake :: Int -> NonEmptyVector a -> Vector a
unsafeTake n = V.unsafeTake n . _neVec

-- ---------------------------------------------------------------------- --
-- Construction

cons :: a -> NonEmptyVector a -> NonEmptyVector a
cons a (NonEmptyVector as) = NonEmptyVector (V.cons a as)
{-# INLINE cons #-}

snoc :: NonEmptyVector a -> a -> NonEmptyVector a
snoc (NonEmptyVector as) a = NonEmptyVector (V.snoc as a)
{-# INLINE snoc #-}

singleton :: a -> NonEmptyVector a
singleton = NonEmptyVector . V.singleton
{-# INLINE singleton #-}

replicate :: Int -> a -> Maybe (NonEmptyVector a)
replicate n a
    | n <= 0 = Nothing
    | otherwise = Just (NonEmptyVector (V.replicate n a))
{-# INLINE replicate #-}

generate :: Int -> (Int -> a) -> Maybe (NonEmptyVector a)
generate n f
    | n <= 0 = Nothing
    | otherwise = Just (NonEmptyVector (V.generate n f))
{-# INLINE generate #-}

iterateN :: Int -> (a -> a) -> a -> Maybe (NonEmptyVector a)
iterateN n f a
    | n <= 0 = Nothing
    | otherwise = Just (NonEmptyVector (V.iterateN n f a))
{-# INLINE iterateN #-}

-- ---------------------------------------------------------------------- --
-- Monadic Initialization

replicateM :: Monad m => Int -> m a -> m (Maybe (NonEmptyVector a))
replicateM n a
    | n <= 0 = return Nothing
    | otherwise = fmap (Just . NonEmptyVector) (V.replicateM n a)

generateM :: Monad m => Int -> (Int -> m a) -> m (Maybe (NonEmptyVector a))
generateM n f
    | n <= 0 = return Nothing
    | otherwise = fmap (Just . NonEmptyVector) (V.generateM n f)

iterateNM :: Monad m => Int -> (a -> m a) -> a -> m (Maybe (NonEmptyVector a))
iterateNM n f a
    | n <= 0 = return Nothing
    | otherwise = fmap (Just . NonEmptyVector) (V.iterateNM n f a)

-- ---------------------------------------------------------------------- --
-- Conversion

fromNonEmpty :: NonEmpty a -> NonEmptyVector a
fromNonEmpty = NonEmptyVector . V.fromList . toList
{-# INLINE fromNonEmpty #-}

toNonEmpty :: NonEmptyVector a -> NonEmpty a
toNonEmpty = NonEmpty.fromList . toList . _neVec
{-# INLINE toNonEmpty #-}

toVector :: NonEmptyVector a -> V.Vector a
toVector = _neVec
{-# INLINE toVector #-}

fromVector :: V.Vector a -> Maybe (NonEmptyVector a)
fromVector v = if V.null v then Nothing else Just (NonEmptyVector v)
{-# INLINE fromVector #-}
