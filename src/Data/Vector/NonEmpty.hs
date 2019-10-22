{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Rank2Types #-}
-- |
-- Module       : Data.Vector.NonEmpty
-- Copyright 	: (c) 2019 Emily Pillmore
-- License	: BSD-style
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: DataTypeable, CPP
--
-- A library for non-empty boxed vectors (that is, polymorphic arrays capable of
-- holding any Haskell value). Non-empty vectors come in two flavors:
--
--  * mutable
--
--  * immutable
--
-- This library attempts to provide support for all standard 'Vector' operations
-- in the API, with some slight variation in types and implementation. For example,
-- since 'head' and 'foldr' are always gauranteed to be over a non-empty 'Vector',
-- it is safe to make use of the 'unsafe-*' 'Vector' operations and semigroupal
-- folds available in the API in lieu of the standard implementations.
--
-- In contrast, some operations such as 'filter' may "break out" of a 'NonEmptyVector'
-- due to the fact that there are no guarantees that may be made on the types of
-- 'Bool'-valued functions passed in, hence one could write the following:
--
-- @
-- filter (const false) v
-- @
--
-- which always produces an empty vector. Thus, some operations must return either
-- a 'Maybe' containing a 'NonEmptyVector' or a 'Vector' whenever appropriate. Generally
-- The former is used in initialization and generation operations, and the latter
-- is used in iterative operations where the intent is not to create an instance
-- of 'NonEmptyVector'.
--
-- Credit to Roman Leshchinskiy for the original Vector library  upon which this is based.
--
module Data.Vector.NonEmpty
( -- * Boxed non-empty vectors
  NonEmptyVector

  -- * Accessors

  -- ** Length information
, length

  -- ** Indexing
, head, last, (!), (!?)
, unsafeIndex

  -- ** Monadic Indexing
, headM, lastM, indexM, unsafeIndexM

  -- ** Extracting subvectors (slicing)
, tail, slice, init, take, drop
, uncons, unsnoc, splitAt
, unsafeSlice, unsafeTake, unsafeDrop

  -- * Construction

  -- ** Initialization
, singleton
, replicate, replicate1
, generate, generate1
, iterateN, iterateN1

  -- ** Monad Initialization
, replicateM, replicate1M
, generateM, generate1M
, iterateNM, iterateN1M
, create, unsafeCreate
, createT, unsafeCreateT

  -- ** Unfolding
, unfoldr, unfoldr1
, unfoldrN, unfoldr1N
, unfoldrM, unfoldr1M
, unfoldrNM, unfoldr1NM
, constructN, constructrN

  -- ** Enumeration
, enumFromN, enumFromStepN
, enumFromTo, enumFromThenTo

  -- ** Concatenation
, cons, snoc, (++), concat, concat1

  -- ** Restricting memory usage
, force

  -- * Conversion

  -- ** To/from non-empty lists
, toNonEmpty, fromNonEmpty
, fromNonEmptyN, fromNonEmptyN1
, unsafeFromList

  -- ** To/from vector
, toVector, fromVector, unsafeFromVector

  -- ** To/from list
, toList, fromList, fromListN

  -- * Modifying non-empty vectors

  -- ** Bulk Updates
, (//), update, update_
, unsafeUpd, unsafeUpdate, unsafeUpdate_

  -- * Accumulations
, accum, accumulate, accumulate_
, unsafeAccum, unsafeAccumulate, unsafeAccumulate_

  -- * Permutations
, reverse, backpermute, unsafeBackpermute

  -- * Safe destructive updates
, modify

  -- * Elementwise operations

  -- ** Indexing
, indexed

  -- ** Mapping
, map, imap, concatMap

  -- ** Monadic mapping
, mapM, imapM, mapM_, imapM_
, forM, forM_

  -- ** Zipping
, zipWith, zipWith3, zipWith4, zipWith5, zipWith6
, izipWith, izipWith3, izipWith4, izipWith5, izipWith6
, zip, zip3, zip4, zip5, zip6

  -- ** Monadic Zipping
, zipWithM, zipWithM_, izipWithM, izipWithM_

  -- ** Unzipping
, unzip, unzip3, unzip4, unzip5, unzip6

  -- * Working with predicates

  -- ** Filtering
, filter, ifilter, uniq, mapMaybe, imapMaybe, filterM
, takeWhile, dropWhile

  -- * Partitioning
, partition, unstablePartition, span, break

  -- * Searching
, elem, notElem, find, findIndex, findIndices, elemIndex
, elemIndices

  -- * Folding
, foldl, foldl1, foldl', foldl1'
, foldr, foldr1, foldr', foldr1'
, ifoldl, ifoldl', ifoldr, ifoldr'

  -- * Specialized folds
, all, any, and, or, sum, product
, maximum, maximumBy, minimum, minimumBy
, maxIndex, maxIndexBy, minIndex, minIndexBy

  -- * Monadic Folds
, foldM, foldM', fold1M, fold1M', foldM_, foldM'_, fold1M_
, fold1M'_, ifoldM, ifoldM', ifoldM_, ifoldM'_

  -- * Monadic Sequencing
, sequence, sequence_

  -- * Prefix sums (scans)
, prescanl, prescanl', postscanl, postscanl'
, scanl, scanl', scanl1, scanl1', iscanl, iscanl'
, prescanr, prescanr', postscanr, postscanr'
, scanr, scanr', scanr1, scanr1', iscanr, iscanr'
) where


import Prelude (Bool, Eq, Ord, Show(..), Num, Enum, (.), Ordering, max)


import Control.Applicative
import Control.DeepSeq hiding (force)
import Control.Monad (Monad, return)
import Control.Monad.ST
import Control.Monad.Zip (MonadZip)

import Data.Data (Data)
import Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable
import Data.Functor
import Data.Functor.Classes (Eq1, Ord1, Show1, Read1(..))
import Data.Int
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (Maybe(..))
import Data.Semigroup (Semigroup(..), (<>))
import Data.Traversable (Traversable, traverse)
import Data.Typeable (Typeable)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import Data.Vector.Mutable (MVector)

import GHC.Read

import qualified Text.Read as Read

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
      else return (unsafeFromList as)

instance Read1 NonEmptyVector where
#if __GLASGOW_HASKELL__ > 802
    liftReadPrec _ rl = do
      l <- rl
      if Foldable.null l
      then Read.pfail
      else return (unsafeFromList l)
#else
    liftReadsPrec _ r _ s = do
      (as, s') <- r s
      if Foldable.null as
      then []
      else return (unsafeFromList as, s')
#endif

instance Foldable NonEmptyVector where
    foldMap f = Foldable.foldMap f . _neVec

instance Traversable NonEmptyVector where
    traverse f = fmap NonEmptyVector . traverse f . _neVec

-- ---------------------------------------------------------------------- --
-- Accessors + Indexing

-- | /O(1)/ Length.
--
length :: NonEmptyVector a -> Int
length = V.length . _neVec
{-# INLINE length #-}

-- | /O(1)/ First element. Since head is gauranteed, bounds checks
-- are bypassed by deferring to 'unsafeHead'.
--
head :: NonEmptyVector a -> a
head = V.unsafeHead . _neVec
{-# INLINE head #-}

-- | /O(1)/ Last element. Since a last element is gauranteed, bounds checks
-- are bypassed by deferring to 'unsafeLast'.
--
last :: NonEmptyVector a -> a
last = V.unsafeLast . _neVec
{-# INLINE last #-}

-- | /O(1)/ Indexing.
--
(!) :: NonEmptyVector a -> Int -> a
(!) (NonEmptyVector as) n = as V.! n
{-# INLINE (!) #-}

-- | /O(1)/ Safe indexing.
--
(!?) :: NonEmptyVector a -> Int -> Maybe a
(NonEmptyVector as) !? n = as V.!? n
{-# INLINE (!?) #-}

-- | /O(1)/ Unsafe indexing without bounds checking
--
unsafeIndex :: NonEmptyVector a -> Int -> a
unsafeIndex (NonEmptyVector as) n = V.unsafeIndex as n
{-# INLINE unsafeIndex #-}

-- ---------------------------------------------------------------------- --
-- Monadic Indexing

-- | /O(1)/ Indexing in a monad.
--
-- The monad allows operations to be strict in the non-empty vector when
-- necessary.
--
-- See 'V.indexM' for more details
--
indexM :: Monad m => NonEmptyVector a -> Int -> m a
indexM (NonEmptyVector v) n = V.indexM v n
{-# INLINE indexM #-}

-- | /O(1)/ First element of a non-empty vector in a monad.
--
-- See 'V.indexM' for an explanation of why this is useful.
--
-- Note that this function defers to 'unsafeHeadM' since head is
-- gauranteed to be safe by construction.
--
headM :: Monad m => NonEmptyVector a -> m a
headM (NonEmptyVector v) = V.unsafeHeadM v
{-# INLINE headM #-}

-- | /O(1)/ Last element of a non-empty vector in a monad. See 'V.indexM' for an
-- explanation of why this is useful.
--
-- Note that this function defers to 'unsafeHeadM' since a last element is
-- gauranteed.
--
lastM :: Monad m => NonEmptyVector a -> m a
lastM (NonEmptyVector v) = V.unsafeLastM v
{-# INLINE lastM #-}

-- | O(1) Indexing in a monad without bounds checks. See 'indexM' for an
-- explanation of why this is useful.
--
unsafeIndexM :: Monad m => NonEmptyVector a -> Int -> m a
unsafeIndexM (NonEmptyVector v) n = V.unsafeIndexM v n
{-# INLINE unsafeIndexM #-}

-- ---------------------------------------------------------------------- --
-- Extracting subvectors (slicing)

-- | /O(1)/ Yield all but the first element without copying. Since the
-- vector returned may be empty (i.e. input was a singleton), this function
-- returns a normal 'Vector'
--
tail :: NonEmptyVector a -> Vector a
tail = V.unsafeTail . _neVec
{-# INLINE tail #-}

-- | /O(1)/ Yield a slice of a non-empty vector without copying at
-- the @0@th and @1@st indices.
--
uncons :: NonEmptyVector a -> (a, Vector a)
uncons v = (head v, tail v)
{-# INLINE uncons #-}

-- | /O(1)/ Yield a slice of a non-empty vector without copying at
-- the @n-1@th and @nth@ indices
--
unsnoc :: NonEmptyVector a -> (Vector a, a)
unsnoc v = (init v, last v)
{-# INLINE unsnoc #-}

-- | /O(1)/ Yield a slice of the non-empty vector without copying it.
-- The vector must contain at least i+n elements. Because this is not
-- guaranteed, this function returns a 'Vector' which could be empty
--
slice :: Int -> Int -> NonEmptyVector a -> Vector a
slice i n = V.slice i n . _neVec

-- | /O(1)/ Yield all but the last element without copying. Since the
-- vector returned may be empty (i.e. input was a singleton), this function
-- returns a normal 'Vector'
--
init :: NonEmptyVector a -> Vector a
init = V.unsafeInit . _neVec
{-# INLINE init #-}

-- | /O(1)/ Yield at the first n elements without copying. The non-empty vector may
-- contain less than n elements in which case it is returned as a vector unchanged.
--
take :: Int -> NonEmptyVector a -> Vector a
take n = V.take n . _neVec
{-# INLINE take #-}

-- | /O(1)/ Yield all but the first n elements without copying. The non-empty vector
-- may contain less than n elements in which case an empty vector is returned.
--
drop :: Int -> NonEmptyVector a -> Vector a
drop n = V.drop n . _neVec
{-# INLINE drop #-}

-- | /O(1)/ Yield the first n elements paired with the remainder without copying.
--
-- This function returns a pair of vectors, as one may slice a (0, n+1).
--
splitAt :: Int -> NonEmptyVector a -> (Vector a, Vector a)
splitAt n = V.splitAt n . _neVec
{-# INLINE splitAt #-}

-- | /O(1)/ Yield a slice of the vector without copying. The vector must contain at
-- least i+n elements but this is not checked.
--
unsafeSlice :: Int -> Int -> NonEmptyVector a -> Vector a
unsafeSlice i n = V.unsafeSlice i n . _neVec
{-# INLINE unsafeSlice #-}

-- | /O(1)/ Yield the first n elements without copying. The vector must contain at
-- least n elements but this is not checked.
--
unsafeTake :: Int -> NonEmptyVector a -> Vector a
unsafeTake n = V.unsafeTake n . _neVec
{-# INLINE unsafeTake #-}

-- | /O(1)/ Yield all but the first n elements without copying. The vector must contain
-- at least n elements but this is not checked.
--
unsafeDrop :: Int -> NonEmptyVector a -> Vector a
unsafeDrop n = V.unsafeDrop n . _neVec
{-# INLINE unsafeDrop #-}

-- ---------------------------------------------------------------------- --
-- Construction

-- | /O(1)/ Non-empty vector with exactly one element
--
singleton :: a -> NonEmptyVector a
singleton = NonEmptyVector . V.singleton
{-# INLINE singleton #-}

-- | /O(n)/ Non-empty vector of the given length with the same value in
-- each position.
--
-- When given a index n <= 0, then 'Nothing' is returned, otherwise 'Just'.
--
replicate :: Int -> a -> Maybe (NonEmptyVector a)
replicate n a = fromVector (V.replicate n a)
{-# INLINE replicate #-}

-- | /O(n)/ Non-empty vector of the given length with the same value in
-- each position.
--
-- This variant takes @max n 1@ for the supplied length parameter.
--
replicate1 :: Int -> a -> NonEmptyVector a
replicate1 n a = unsafeFromVector (V.replicate (max n 1) a)
{-# INLINE replicate1 #-}

-- | /O(n)/ Construct a vector of the given length by applying the function to
-- each index.
--
-- When given a index n <= 0, then 'Nothing' is returned, otherwise 'Just'.
--
generate :: Int -> (Int -> a) -> Maybe (NonEmptyVector a)
generate n f = fromVector (V.generate n f)
{-# INLINE generate #-}

-- | /O(n)/ Construct a vector of the given length by applying the function to
-- each index.
--
-- This variant takes @max n 1@ for the supplied length parameter.
--
generate1 :: Int -> (Int -> a) -> NonEmptyVector a
generate1 n f = unsafeFromVector (V.generate (max n 1) f)
{-# INLINE generate1 #-}

-- | /O(n)/ Apply function n times to value. Zeroth element is original value.
--
-- When given a index n <= 0, then 'Nothing' is returned, otherwise 'Just'.
--
iterateN :: Int -> (a -> a) -> a -> Maybe (NonEmptyVector a)
iterateN n f a = fromVector (V.iterateN n f a)
{-# INLINE iterateN #-}

-- | /O(n)/ Apply function n times to value. Zeroth element is original value.
--
-- This variant takes @max n 1@ for the supplied length parameter.
--
iterateN1 :: Int -> (a -> a) -> a -> NonEmptyVector a
iterateN1 n f a = unsafeFromVector (V.iterateN (max n 1) f a)
{-# INLINE iterateN1 #-}

-- ---------------------------------------------------------------------- --
-- Monadic Initialization

-- | /O(n)/ Execute the monadic action the given number of times and store
-- the results in a vector.
--
-- When given a index n <= 0, then 'Nothing' is returned, otherwise 'Just'.
--
replicateM :: Monad m => Int -> m a -> m (Maybe (NonEmptyVector a))
replicateM n a = fmap fromVector (V.replicateM n a)
{-# INLINE replicateM #-}

-- | /O(n)/ Execute the monadic action the given number of times and store
-- the results in a vector.
--
-- This variant takes @max n 1@ for the supplied length parameter.
--
replicate1M :: Monad m => Int -> m a -> m (NonEmptyVector a)
replicate1M n a = fmap unsafeFromVector (V.replicateM (max n 1) a)
{-# INLINE replicate1M #-}

-- | /O(n)/ Construct a vector of the given length by applying the monadic
-- action to each index
--
-- When given a index n <= 0, then 'Nothing' is returned, otherwise 'Just'.
--
generateM :: Monad m => Int -> (Int -> m a) -> m (Maybe (NonEmptyVector a))
generateM n f = fmap fromVector (V.generateM n f)
{-# INLINE generateM #-}

-- | /O(n)/ Construct a vector of the given length by applying the monadic
-- action to each index
--
-- This variant takes @max n 1@ for the supplied length parameter.
--
generate1M :: Monad m => Int -> (Int -> m a) -> m (NonEmptyVector a)
generate1M n f = fmap unsafeFromVector (V.generateM (max n 1) f)
{-# INLINE generate1M #-}

-- | /O(n)/ Apply monadic function n times to value. Zeroth element is
-- original value.
--
-- When given a index n <= 0, then 'Nothing' is returned, otherwise 'Just'.
--
iterateNM :: Monad m => Int -> (a -> m a) -> a -> m (Maybe (NonEmptyVector a))
iterateNM n f a = fmap fromVector (V.iterateNM n f a)
{-# INLINE iterateNM #-}

-- | /O(n)/ Apply monadic function n times to value. Zeroth element is
-- original value.
--
-- This variant takes @max n 1@ for the supplied length parameter.
--
iterateN1M :: Monad m => Int -> (a -> m a) -> a -> m (NonEmptyVector a)
iterateN1M n f a = fmap unsafeFromVector (V.iterateNM (max n 1) f a)
{-# INLINE iterateN1M #-}

-- | Execute the monadic action and freeze the resulting non-empty vector.
--
create :: (forall s. ST s (MVector s a)) -> Maybe (NonEmptyVector a)
create p = fromVector (G.create p)
{-# INLINE create #-}

-- | Execute the monadic action and freeze the resulting non-empty vector,
-- bypassing emptiness checks.
--
-- The onus is on the caller to guarantee the created vector is non-empty.
--
unsafeCreate :: (forall s. ST s (MVector s a)) -> NonEmptyVector a
unsafeCreate p = unsafeFromVector (G.create p)
{-# INLINE unsafeCreate #-}

-- | Execute the monadic action and freeze the resulting non-empty vector.
--
createT
    :: Traversable t
    => (forall s. ST s (t (MVector s a)))
    -> t (Maybe (NonEmptyVector a))
createT p = fmap fromVector (G.createT p)
{-# INLINE createT #-}

-- | Execute the monadic action and freeze the resulting non-empty vector.
--
-- The onus is on the caller to guarantee the created vector is non-empty.
--
unsafeCreateT
    :: Traversable t
    => (forall s. ST s (t (MVector s a)))
    -> t (NonEmptyVector a)
unsafeCreateT p = fmap unsafeFromVector (G.createT p)
{-# INLINE unsafeCreateT #-}

-- ---------------------------------------------------------------------- --
-- Unfolding

-- | /O(n)/ Construct a non-empty vector by repeatedly applying the
-- generator function to a seed. The generator function yields 'Just' the
-- next element and the new seed or 'Nothing' if there are no more
-- elements.
--
-- If an unfold does not create meaningful values, 'Nothing' is
-- returned. Otherwise, 'Just' containing a non-empty vector is returned.
--
unfoldr :: (b -> Maybe (a, b)) -> b -> Maybe (NonEmptyVector a)
unfoldr f b = fromVector (V.unfoldr f b)
{-# INLINE unfoldr #-}

-- | /O(n)/ Construct a non-empty vector by repeatedly applying the
-- generator function to a seed and a first element.
--
-- This variant of 'unfoldr' guarantees the resulting vector is non-
-- empty by supplying an initial element @a@.
--
unfoldr1 :: (b -> Maybe (a, b)) -> a -> b -> NonEmptyVector a
unfoldr1 f a b = cons a (unsafeFromVector (V.unfoldr f b))
{-# INLINE unfoldr1 #-}

-- | /O(n)/ Construct a vector with at most n elements by repeatedly
-- applying the generator function to a seed. The generator function yields
-- 'Just' the next element and the new seed or 'Nothing' if there are no
-- more elements.
--
-- If an unfold does not create meaningful values, 'Nothing' is
-- returned. Otherwise, 'Just' containing a non-empty vector is returned.
--
unfoldrN :: Int -> (b -> Maybe (a, b)) -> b -> Maybe (NonEmptyVector a)
unfoldrN n f b = fromVector (V.unfoldrN n f b)
{-# INLINE unfoldrN #-}

-- | /O(n)/ Construct a vector with at most n elements by repeatedly
-- applying the generator function to a seed. The generator function yields
-- 'Just' the next element and the new seed or 'Nothing' if there are no
-- more elements.
--
-- This variant of 'unfoldrN' guarantees the resulting vector is non-
-- empty by supplying an initial element @a@.
--
unfoldr1N
    :: Int
    -> (b -> Maybe (a, b))
    -> a
    -> b
    -> NonEmptyVector a
unfoldr1N n f a b = cons a (unsafeFromVector (V.unfoldrN n f b))
{-# INLINE unfoldr1N #-}

-- | /O(n)/ Construct a non-empty vector by repeatedly applying the monadic generator
-- function to a seed. The generator function yields Just the next element
-- and the new seed or Nothing if there are no more elements.
--
-- If an unfold does not create meaningful values, 'Nothing' is
-- returned. Otherwise, 'Just' containing a non-empty vector is returned.
--
unfoldrM
    :: Monad m
    => (b -> m (Maybe (a, b)))
    -> b
    -> m (Maybe (NonEmptyVector a))
unfoldrM f b = fmap fromVector (V.unfoldrM f b)
{-# INLINE unfoldrM #-}

-- | /O(n)/ Construct a non-empty vector by repeatedly applying the monadic generator
-- function to a seed. The generator function yields Just the next element
-- and the new seed or Nothing if there are no more elements.
--
-- This variant of 'unfoldrM' guarantees the resulting vector is non-
-- empty by supplying an initial element @a@.
--
unfoldr1M
    :: Monad m
    => (b -> m (Maybe (a, b)))
    -> a
    -> b
    -> m (NonEmptyVector a)
unfoldr1M f a b = fmap (cons a . unsafeFromVector) (V.unfoldrM f b)
{-# INLINE unfoldr1M #-}

-- | /O(n)/ Construct a non-empty vector by repeatedly applying the monadic generator
-- function to a seed. The generator function yields Just the next element and
-- the new seed or Nothing if there are no more elements.
--
-- If an unfold does not create meaningful values, 'Nothing' is
-- returned. Otherwise, 'Just' containing a non-empty vector is returned.
--
unfoldrNM
    :: Monad m
    => Int
    -> (b -> m (Maybe (a, b)))
    -> b
    -> m (Maybe (NonEmptyVector a))
unfoldrNM n f b = fmap fromVector (V.unfoldrNM n f b)
{-# INLINE unfoldrNM #-}

-- | /O(n)/ Construct a non-empty vector by repeatedly applying the monadic generator
-- function to a seed. The generator function yields Just the next element and
-- the new seed or Nothing if there are no more elements.
--
-- This variant of 'unfoldrNM' guarantees the resulting vector is non-
-- empty by supplying an initial element @a@.
--
unfoldr1NM
    :: Monad m
    => Int
    -> (b -> m (Maybe (a, b)))
    -> a
    -> b
    -> m (NonEmptyVector a)
unfoldr1NM n f a b = fmap (cons a . unsafeFromVector) (V.unfoldrNM n f b)
{-# INLINE unfoldr1NM #-}

-- | /O(n)/ Construct a non-empty vector with n elements by repeatedly applying the
-- generator function to the already constructed part of the vector.
--
-- If 'constructN' does not create meaningful values, 'Nothing' is
-- returned. Otherwise, 'Just' containing a non-empty vector is returned.
--
constructN :: Int -> (Vector a -> a) -> Maybe (NonEmptyVector a)
constructN n f = fromVector (V.constructN n f)
{-# INLINE constructN #-}

-- | /O(n)/ Construct a vector with n elements from right to left by repeatedly
-- applying the generator function to the already constructed part of the vector.
--
-- If 'constructrN' does not create meaningful values, 'Nothing' is
-- returned. Otherwise, 'Just' containing a non-empty vector is returned.
--
constructrN :: Int -> (Vector a -> a) -> Maybe (NonEmptyVector a)
constructrN n f = fromVector (V.constructrN n f)
{-# INLINE constructrN #-}

-- ---------------------------------------------------------------------- --
-- Enumeration

-- | /O(n)/ Yield a non-emptyvector of the given length containing the
-- values x, x+1 etc. This operation is usually more efficient than
-- 'enumFromTo'.
--
-- If an enumeration does not use meaningful indices, 'Nothing' is returned,
-- otherwise, 'Just' containing a non-empty vector.
--
enumFromN :: Num a => a -> Int -> Maybe (NonEmptyVector a)
enumFromN a n = fromVector (V.enumFromN a n)
{-# INLINE enumFromN #-}

-- | /O(n)/ Yield a non-empty vector of the given length containing the
-- values x, x+y, x+y+y etc. This operations is usually more efficient than
-- 'enumFromThenTo'.
--
-- If an enumeration does not use meaningful indices, 'Nothing' is returned,
-- otherwise, 'Just' containing a non-empty vector.
--
enumFromStepN :: Num a => a -> a -> Int -> Maybe (NonEmptyVector a)
enumFromStepN a0 a1 n = fromVector (V.enumFromStepN a0 a1 n)
{-# INLINE enumFromStepN #-}

-- | /O(n)/ Enumerate values from x to y.
--
-- If an enumeration does not use meaningful indices, 'Nothing' is returned,
-- otherwise, 'Just' containing a non-empty vector.
--
-- /WARNING/: This operation can be very inefficient. If at all possible,
-- use 'enumFromN' instead.
--
--
enumFromTo :: Enum a => a -> a -> Maybe (NonEmptyVector a)
enumFromTo a0 a1 = fromVector (V.enumFromTo a0 a1)
{-# INLINE enumFromTo #-}

-- | /O(n)/ Enumerate values from x to y with a specific step z.
--
-- If an enumeration does not use meaningful indices, 'Nothing' is returned,
-- otherwise, 'Just' containing a non-empty vector.
--
-- /WARNING/: This operation can be very inefficient. If at all possible,
-- use 'enumFromStepN' instead.
enumFromThenTo :: Enum a => a -> a -> a -> Maybe (NonEmptyVector a)
enumFromThenTo a0 a1 a2 = fromVector (V.enumFromThenTo a0 a1 a2)
{-# INLINE enumFromThenTo #-}

-- ---------------------------------------------------------------------- --
-- Concatenation

-- | /O(n)/ Prepend an element
--
cons :: a -> NonEmptyVector a -> NonEmptyVector a
cons a (NonEmptyVector as) = NonEmptyVector (V.cons a as)
{-# INLINE cons #-}

-- | /O(n)/ Append an element
--
snoc :: NonEmptyVector a -> a -> NonEmptyVector a
snoc (NonEmptyVector as) a = NonEmptyVector (V.snoc as a)
{-# INLINE snoc #-}

-- | /O(m+n)/ Concatenate two non-empty vectors
--
(++) :: NonEmptyVector a -> NonEmptyVector a -> NonEmptyVector a
NonEmptyVector v ++ NonEmptyVector v' = NonEmptyVector (v <> v')
{-# INLINE (++) #-}

-- | /O(n)/ Concatenate all non-empty vectors in the list
--
-- If list is empty, 'Nothing' is returned, otherwise 'Just'
-- containing the concatenated non-empty vectors
--
concat :: [NonEmptyVector a] -> Maybe (NonEmptyVector a)
concat [] = Nothing
concat (a:as) = Just (concat1 (a :| as))
{-# INLINE concat #-}

-- | O(n) Concatenate all non-empty vectors in a non-empty list.
--
concat1 :: NonEmpty (NonEmptyVector a) -> NonEmptyVector a
concat1 = NonEmptyVector . Foldable.foldl' go V.empty
  where
    go v (NonEmptyVector a) = v <> a
{-# INLINE concat1 #-}

-- ---------------------------------------------------------------------- --
-- Conversions

-- | /O(n)/ Convert a non-empty vector to a non-empty list.
--
toNonEmpty :: NonEmptyVector a -> NonEmpty a
toNonEmpty = NonEmpty.fromList . V.toList . _neVec
{-# INLINE toNonEmpty #-}

-- | O(n) Convert from a non-empty list to a non-empty vector.
--
fromNonEmpty :: NonEmpty a -> NonEmptyVector a
fromNonEmpty = NonEmptyVector . V.fromList . Foldable.toList
{-# INLINE fromNonEmpty #-}

-- | O(n) Convert from the first n-elements of a non-empty list to a
-- non-empty vector.
--
-- Returns 'Nothing' if indices are <= 0, otherwise 'Just' containing
-- the non-empty vector.
--
fromNonEmptyN :: Int -> NonEmpty a -> Maybe (NonEmptyVector a)
fromNonEmptyN n a = fromVector (V.fromListN n (Foldable.toList a))
{-# INLINE fromNonEmptyN #-}

-- | O(n) Convert from the first n-elements of a non-empty list to a
-- non-empty vector. This is a safe version of `fromNonEmptyN` which
-- takes @max n 1@ of the first n-elements of the non-empty list.
--
fromNonEmptyN1 :: Int -> NonEmpty a -> NonEmptyVector a
fromNonEmptyN1 n = unsafeFromVector . V.fromListN (max n 1) . Foldable.toList
{-# INLINE fromNonEmptyN1 #-}

-- | /O(1)/ Convert from a non-empty vector to a vector.
--
toVector :: NonEmptyVector a -> V.Vector a
toVector = _neVec
{-# INLINE toVector #-}

-- | /O(1)/ Convert from a vector to a non-empty vector.
--
-- If the vector is empty, then 'Nothing' is returned,
-- otherwise 'Just' containing the non-empty vector.
--
fromVector :: Vector a -> Maybe (NonEmptyVector a)
fromVector v = if V.null v then Nothing else Just (NonEmptyVector v)
{-# INLINE fromVector #-}

-- | /O(1)/ Convert from a vector to a non-empty vector without
-- checking bounds.
--
-- /Warning/: the onus is on the user to ensure that their vector
-- is not empty, otherwise all bets are off!
--
unsafeFromVector :: Vector a -> NonEmptyVector a
unsafeFromVector = NonEmptyVector
{-# INLINE unsafeFromVector #-}

-- | /O(n)/ Convert from a non-empty vector to a list.
--
toList :: NonEmptyVector a -> [a]
toList = V.toList . _neVec
{-# INLINE toList #-}

-- | /O(n)/ Convert from a list to a non-empty vector.
--
fromList :: [a] -> Maybe (NonEmptyVector a)
fromList = fromVector . V.fromList
{-# INLINE fromList #-}

-- | /O(n)/ Convert from a list to a non-empty vector.
--
-- /Warning/: the onus is on the user to ensure that their vector
-- is not empty, otherwise all bets are off!
unsafeFromList :: [a] -> NonEmptyVector a
unsafeFromList = unsafeFromVector . V.fromList
{-# INLINE unsafeFromList #-}

-- | /O(n)/ Convert the first n elements of a list to a non-empty vector.
--
-- If the list is empty or <= 0 elements are chosen, 'Nothing' is
-- returned, otherwise 'Just' containing the non-empty vector
--
fromListN :: Int -> [a] -> Maybe (NonEmptyVector a)
fromListN n as = fromVector (V.fromListN n as)
{-# INLINE fromListN #-}

-- ---------------------------------------------------------------------- --
-- Restricting memory usage

-- | /O(n)/ Yield the argument but force it not to retain any extra memory,
-- possibly by copying it.
--
force :: NonEmptyVector a -> NonEmptyVector a
force (NonEmptyVector a) = NonEmptyVector (V.force a)
{-# INLINE force #-}

-- ---------------------------------------------------------------------- --
-- Bulk Updates

-- | /O(m+n)/ For each pair (i,a) from the list, replace the non-empty vector
-- element at position i by a.
--
(//) :: NonEmptyVector a -> [(Int, a)] -> NonEmptyVector a
NonEmptyVector v // us = NonEmptyVector (v V.// us)
{-# INLINE (//) #-}

-- | O(m+n) For each pair (i,a) from the vector of index/value pairs,
-- replace the vector element at position i by a.
--
update :: NonEmptyVector a -> Vector (Int, a) -> NonEmptyVector a
update (NonEmptyVector v) v' = NonEmptyVector (V.update v v')
{-# INLINE update #-}

-- | /O(m+min(n1,n2))/ For each index i from the index vector and the
-- corresponding value a from the value vector, replace the element of
-- the initial vector at position i by a.
--
update_ :: NonEmptyVector a -> Vector Int -> Vector a -> NonEmptyVector a
update_ (NonEmptyVector v) is as = NonEmptyVector (V.update_ v is as)
{-# INLINE update_ #-}

-- | Same as '(//)' but without bounds checking.
--
unsafeUpd :: NonEmptyVector a -> [(Int, a)] -> NonEmptyVector a
unsafeUpd (NonEmptyVector v) us = NonEmptyVector (V.unsafeUpd v us)
{-# INLINE unsafeUpd #-}

-- | Same as 'update' but without bounds checking.
--
unsafeUpdate :: NonEmptyVector a -> Vector (Int, a) -> NonEmptyVector a
unsafeUpdate (NonEmptyVector v) us = NonEmptyVector (V.unsafeUpdate v us)
{-# INLINE unsafeUpdate #-}

-- | Same as 'update_' but without bounds checking.
--
unsafeUpdate_ :: NonEmptyVector a -> Vector Int -> Vector a -> NonEmptyVector a
unsafeUpdate_ (NonEmptyVector v) is as = NonEmptyVector (V.unsafeUpdate_ v is as)
{-# INLINE unsafeUpdate_ #-}

-- ---------------------------------------------------------------------- --
-- Accumulation

-- | /O(m+n)/ For each pair @(i,b)@ from the non-empty list, replace the
-- non-empty vector element @a@ at position @i@ by @f a b@.
--
accum
    :: (a -> b -> a)
      -- ^ accumulating function @f@
    -> NonEmptyVector a
      -- ^ initial non-empty vector (of length @m@)
    -> [(Int, b)]
      -- ^ list of index/value pairs (of length @n@)
    -> NonEmptyVector a
accum f (NonEmptyVector v) u = NonEmptyVector (V.accum f v u)

{-# INLINE accum #-}

-- | /O(m+n)/ For each pair @(i,b)@ from the vector of pairs, replace the
-- non-empty vector element @a@ at position @i@ by @f a b@.
--
accumulate
    :: (a -> b -> a)
      -- ^ accumulating function @f@
    -> NonEmptyVector a
      -- ^ initial non-empty vector (of length @m@)
    -> Vector (Int, b)
      -- ^ vector of index/value pairs (of length @n@)
    -> NonEmptyVector a
accumulate f (NonEmptyVector v) u = NonEmptyVector (V.accumulate f v u)
{-# INLINE accumulate #-}

-- | /O(m+min(n1,n2))/ For each index @i@ from the index vector and the
-- corresponding value @b@ from the the value vector, replace the element
-- of the initial non-empty vector at position @i@ by @f a b@.
--
accumulate_
    :: (a -> b -> a)
      -- ^ accumulating function @f@
    -> NonEmptyVector a
      -- ^ initial non-empty vector (of length @m@)
    -> Vector Int
       -- ^ vector of indices (of length @n1@)
    -> Vector b
       -- ^ vector of values (of length @n2@)
    -> NonEmptyVector a
accumulate_ f (NonEmptyVector v) i b = NonEmptyVector (V.accumulate_ f v i b)
{-# INLINE accumulate_ #-}

-- | Same as 'accum' but without bounds checking.
--
unsafeAccum
    :: (a -> b -> a)
      -- ^ accumulating function @f@
    -> NonEmptyVector a
      -- ^ initial non-empty vector (of length @m@)
    -> [(Int, b)]
      -- ^ list of index/value pairs (of length @n@)
    -> NonEmptyVector a
unsafeAccum f (NonEmptyVector v) u = NonEmptyVector (V.unsafeAccum f v u)
{-# INLINE unsafeAccum #-}

-- | Same as 'accumulate' but without bounds checking.
--
unsafeAccumulate
    :: (a -> b -> a)
      -- ^ accumulating function @f@
    -> NonEmptyVector a
      -- ^ initial non-empty vector (of length @m@)
    -> Vector (Int, b)
      -- ^ vector of index/value pairs (of length @n@)
    -> NonEmptyVector a
unsafeAccumulate f v u = NonEmptyVector (V.unsafeAccumulate f v' u)
  where
    v' = _neVec v
{-# INLINE unsafeAccumulate #-}

-- | Same as 'accumulate_' but without bounds checking.
--
unsafeAccumulate_
    :: (a -> b -> a)
      -- ^ accumulating function @f@
    -> NonEmptyVector a
      -- ^ initial non-empty vector (of length @m@)
    -> Vector Int
      -- ^ vector of indices of length @n1@
    -> Vector b
      -- ^ vector of values (of length @n2@)
    -> NonEmptyVector a
unsafeAccumulate_ f v i b = NonEmptyVector (V.unsafeAccumulate_ f v' i b)
  where
    v' = _neVec v
{-# INLINE unsafeAccumulate_ #-}

-- ---------------------------------------------------------------------- --
-- Permutations

-- | /O(n)/ Reverse a non-empty vector
--
reverse :: NonEmptyVector a -> NonEmptyVector a
reverse = NonEmptyVector . V.reverse . _neVec
{-# INLINE reverse #-}

-- | /O(n)/ Yield the non-empty vector obtained by replacing each element
-- @i@ of the non-empty index vector by @xs'!'i@. This is equivalent to
-- @'map' (xs'!') is@ but is often much more efficient.
--
backpermute :: NonEmptyVector a -> NonEmptyVector Int -> NonEmptyVector a
backpermute (NonEmptyVector v) (NonEmptyVector i)
    = NonEmptyVector (V.backpermute v i)
{-# INLINE backpermute #-}

-- | Same as 'backpermute' but without bounds checking.
--
unsafeBackpermute
    :: NonEmptyVector a
    -> NonEmptyVector Int
    -> NonEmptyVector a
unsafeBackpermute (NonEmptyVector v) (NonEmptyVector i)
    = NonEmptyVector (V.unsafeBackpermute v i)
{-# INLINE unsafeBackpermute #-}

-- ---------------------------------------------------------------------- --
-- Safe destructive updates

-- | Apply a destructive operation to a non-empty vector. The operation
-- will be performed in place if it is safe to do so and will modify a
-- copy of the non-empty vector otherwise.
--
modify
    :: (forall s. MVector s a -> ST s ())
    -> NonEmptyVector a
    -> NonEmptyVector a
modify p (NonEmptyVector v) = NonEmptyVector (V.modify p v)
{-# INLINE modify #-}

-- ---------------------------------------------------------------------- --
-- Indexing

-- | /O(n)/ Pair each element in a vector with its index.
--
indexed :: NonEmptyVector a -> NonEmptyVector (Int, a)
indexed = NonEmptyVector . V.indexed . _neVec
{-# INLINE indexed #-}

-- ---------------------------------------------------------------------- --
-- Mapping

-- | /O(n)/ Map a function over a non-empty vector.
--
map :: (a -> b) -> NonEmptyVector a -> NonEmptyVector b
map f = NonEmptyVector . V.map f . _neVec
{-# INLINE map #-}

-- | /O(n)/ Apply a function to every element of a non-empty vector and
-- its index.
--
imap :: (Int -> a -> b) -> NonEmptyVector a -> NonEmptyVector b
imap f = NonEmptyVector . V.imap f . _neVec
{-# INLINE imap #-}

-- | Map a function over a vector and concatenate the results.
--
concatMap
    :: (a -> NonEmptyVector b)
    -> NonEmptyVector a
    -> NonEmptyVector b
concatMap f = NonEmptyVector . V.concatMap (_neVec . f) . _neVec
{-# INLINE concatMap #-}

-- ---------------------------------------------------------------------- --
-- Monadic Mapping

-- | /O(n)/ Apply the monadic action to all elements of the non-empty
-- vector, yielding non-empty vector of results.
--
mapM :: Monad m => (a -> m b) -> NonEmptyVector a -> m (NonEmptyVector b)
mapM f = fmap NonEmptyVector . V.mapM f . _neVec
{-# INLINE mapM #-}

-- | /O(n)/ Apply the monadic action to every element of a non-empty
-- vector and its index, yielding a non-empty vector of results.
--
imapM
    :: Monad m
    => (Int -> a -> m b)
    -> NonEmptyVector a
    -> m (NonEmptyVector b)
imapM f = fmap NonEmptyVector . V.imapM f . _neVec
{-# INLINE imapM #-}

-- | /O(n)/ Apply the monadic action to all elements of a non-empty vector
-- and ignore the results.
--
mapM_ :: Monad m => (a -> m b) -> NonEmptyVector a -> m ()
mapM_ f = V.mapM_ f . _neVec
{-# INLINE mapM_ #-}

-- | /O(n)/ Apply the monadic action to every element of a non-emptpy
-- vector and its index, ignoring the results
--
imapM_ :: Monad m => (Int -> a -> m b) -> NonEmptyVector a -> m ()
imapM_ f = V.imapM_ f . _neVec
{-# INLINE imapM_ #-}

-- | /O(n)/ Apply the monadic action to all elements of the non-empty
-- vector, yielding a  non0empty vector of results.
--
-- Equivalent to @flip 'mapM'@.
--
forM :: Monad m => NonEmptyVector a -> (a -> m b) -> m (NonEmptyVector b)
forM (NonEmptyVector v) f = fmap NonEmptyVector (V.forM v f)
{-# INLINE forM #-}

-- | /O(n)/ Apply the monadic action to all elements of a non-empty
-- vector and ignore the results.
--
-- Equivalent to @flip 'mapM_'@.
--
forM_ :: Monad m => NonEmptyVector a -> (a -> m b) -> m ()
forM_ (NonEmptyVector v) f = V.forM_ v f
{-# INLINE forM_ #-}

-- ---------------------------------------------------------------------- --
-- Zipping

-- | /O(min(m,n))/ Zip two non-empty vectors with the given function.
--
zipWith
    :: (a -> b -> c)
    -> NonEmptyVector a
    -> NonEmptyVector b
    -> NonEmptyVector c
zipWith f a b = NonEmptyVector (V.zipWith f a' b')
  where
    a' = _neVec a
    b' = _neVec b
{-# INLINE zipWith #-}

-- | Zip three non-empty vectors with the given function.
--
zipWith3
    :: (a -> b -> c -> d)
    -> NonEmptyVector a
    -> NonEmptyVector b
    -> NonEmptyVector c
    -> NonEmptyVector d
zipWith3 f a b c = NonEmptyVector (V.zipWith3 f a' b' c')
  where
    a' = _neVec a
    b' = _neVec b
    c' = _neVec c
{-# INLINE zipWith3 #-}

-- | Zip four non-empty vectors with the given function.
--
zipWith4
    :: (a -> b -> c -> d -> e)
    -> NonEmptyVector a
    -> NonEmptyVector b
    -> NonEmptyVector c
    -> NonEmptyVector d
    -> NonEmptyVector e
zipWith4 f a b c d = NonEmptyVector (V.zipWith4 f a' b' c' d')
  where
    a' = _neVec a
    b' = _neVec b
    c' = _neVec c
    d' = _neVec d
{-# INLINE zipWith4 #-}

-- | Zip five non-empty vectors with the given function.
--
zipWith5
    :: (a -> b -> c -> d -> e -> f)
    -> NonEmptyVector a
    -> NonEmptyVector b
    -> NonEmptyVector c
    -> NonEmptyVector d
    -> NonEmptyVector e
    -> NonEmptyVector f
zipWith5 f a b c d e = NonEmptyVector (V.zipWith5 f a' b' c' d' e')
  where
    a' = _neVec a
    b' = _neVec b
    c' = _neVec c
    d' = _neVec d
    e' = _neVec e
{-# INLINE zipWith5 #-}

-- | Zip six non-empty vectors with the given function.
--
zipWith6
    :: (a -> b -> c -> d -> e -> f -> g)
    -> NonEmptyVector a
    -> NonEmptyVector b
    -> NonEmptyVector c
    -> NonEmptyVector d
    -> NonEmptyVector e
    -> NonEmptyVector f
    -> NonEmptyVector g
zipWith6 k a b c d e f = NonEmptyVector (V.zipWith6 k a' b' c' d' e' f')
  where
    a' = _neVec a
    b' = _neVec b
    c' = _neVec c
    d' = _neVec d
    e' = _neVec e
    f' = _neVec f
{-# INLINE zipWith6 #-}


-- | /O(min(m,n))/ Zip two non-empty vectors with a function that also
-- takes the elements' indices.
--
izipWith
    :: (Int -> a -> b -> c)
    -> NonEmptyVector a
    -> NonEmptyVector b
    -> NonEmptyVector c
izipWith f a b = NonEmptyVector (V.izipWith f a' b')
  where
    a' = _neVec a
    b' = _neVec b
{-# INLINE izipWith #-}

-- | Zip three non-empty vectors and their indices with the given function.
--
izipWith3
    :: (Int -> a -> b -> c -> d)
    -> NonEmptyVector a
    -> NonEmptyVector b
    -> NonEmptyVector c
    -> NonEmptyVector d
izipWith3 f a b c = NonEmptyVector (V.izipWith3 f a' b' c')
  where
    a' = _neVec a
    b' = _neVec b
    c' = _neVec c
{-# INLINE izipWith3 #-}

-- | Zip four non-empty vectors and their indices with the given function.
--
izipWith4
    :: (Int -> a -> b -> c -> d -> e)
    -> NonEmptyVector a
    -> NonEmptyVector b
    -> NonEmptyVector c
    -> NonEmptyVector d
    -> NonEmptyVector e
izipWith4 f a b c d = NonEmptyVector (V.izipWith4 f a' b' c' d')
  where
    a' = _neVec a
    b' = _neVec b
    c' = _neVec c
    d' = _neVec d
{-# INLINE izipWith4 #-}

-- | Zip five non-empty vectors and their indices with the given function.
--
izipWith5
    :: (Int -> a -> b -> c -> d -> e -> f)
    -> NonEmptyVector a
    -> NonEmptyVector b
    -> NonEmptyVector c
    -> NonEmptyVector d
    -> NonEmptyVector e
    -> NonEmptyVector f
izipWith5 f a b c d e = NonEmptyVector (V.izipWith5 f a' b' c' d' e')
  where
    a' = _neVec a
    b' = _neVec b
    c' = _neVec c
    d' = _neVec d
    e' = _neVec e
{-# INLINE izipWith5 #-}

-- | Zip six non-empty vectors and their indices with the given function.
--
izipWith6
    :: (Int -> a -> b -> c -> d -> e -> f -> g)
    -> NonEmptyVector a
    -> NonEmptyVector b
    -> NonEmptyVector c
    -> NonEmptyVector d
    -> NonEmptyVector e
    -> NonEmptyVector f
    -> NonEmptyVector g
izipWith6 k a b c d e f = NonEmptyVector (V.izipWith6 k a' b' c' d' e' f')
  where
    a' = _neVec a
    b' = _neVec b
    c' = _neVec c
    d' = _neVec d
    e' = _neVec e
    f' = _neVec f
{-# INLINE izipWith6 #-}

-- | /O(min(n,m))/ Elementwise pairing of non-empty vector elements.
--
zip :: NonEmptyVector a -> NonEmptyVector b -> NonEmptyVector (a, b)
zip a b = NonEmptyVector (V.zip a' b')
  where
    a' = _neVec a
    b' = _neVec b
{-# INLINE zip #-}

-- | Zip together three non-empty vectors.
--
zip3
    :: NonEmptyVector a
    -> NonEmptyVector b
    -> NonEmptyVector c
    -> NonEmptyVector (a, b, c)
zip3 a b c = NonEmptyVector (V.zip3 a' b' c')
  where
    a' = _neVec a
    b' = _neVec b
    c' = _neVec c
{-# INLINE zip3 #-}

-- | Zip together four non-empty vectors.
--
zip4
    :: NonEmptyVector a
    -> NonEmptyVector b
    -> NonEmptyVector c
    -> NonEmptyVector d
    -> NonEmptyVector (a, b, c, d)
zip4 a b c d = NonEmptyVector (V.zip4 a' b' c' d')
  where
    a' = _neVec a
    b' = _neVec b
    c' = _neVec c
    d' = _neVec d
{-# INLINE zip4 #-}

-- | Zip together five non-empty vectors.
--
zip5
    :: NonEmptyVector a
    -> NonEmptyVector b
    -> NonEmptyVector c
    -> NonEmptyVector d
    -> NonEmptyVector e
    -> NonEmptyVector (a, b, c, d, e)
zip5 a b c d e = NonEmptyVector (V.zip5 a' b' c' d' e')
  where
    a' = _neVec a
    b' = _neVec b
    c' = _neVec c
    d' = _neVec d
    e' = _neVec e
{-# INLINE zip5 #-}

-- | Zip together six non-empty vectors.
--
zip6
    :: NonEmptyVector a
    -> NonEmptyVector b
    -> NonEmptyVector c
    -> NonEmptyVector d
    -> NonEmptyVector e
    -> NonEmptyVector f
    -> NonEmptyVector (a, b, c, d, e, f)
zip6 a b c d e f = NonEmptyVector (V.zip6 a' b' c' d' e' f')
  where
    a' = _neVec a
    b' = _neVec b
    c' = _neVec c
    d' = _neVec d
    e' = _neVec e
    f' = _neVec f
{-# INLINE zip6 #-}

-- ---------------------------------------------------------------------- --
-- Monadic Zipping

-- | /O(min(m,n))/ Zip the two non-empty vectors with the monadic action
-- and yield a non-empty vector of results.
--
zipWithM
    :: Monad m
    => (a -> b -> m c)
    -> NonEmptyVector a
    -> NonEmptyVector b
    -> m (NonEmptyVector c)
zipWithM f a b = fmap NonEmptyVector (V.zipWithM f a' b')
  where
    a' = _neVec a
    b' = _neVec b
{-# INLINE zipWithM #-}

-- | /O(min(m,n))/ Zip the two non-empty vectors with a monadic action
-- that also takes the element index and yield a vector of results.
--
izipWithM
    :: Monad m
    => (Int -> a -> b -> m c)
    -> NonEmptyVector a
    -> NonEmptyVector b
    -> m (NonEmptyVector c)
izipWithM f a b = fmap NonEmptyVector (V.izipWithM f a' b')
  where
    a' = _neVec a
    b' = _neVec b
{-# INLINE izipWithM #-}

-- | /O(min(m,n))/ Zip the two non-empty vectors with the monadic action
-- and ignore the results.
--
zipWithM_
    :: Monad m
    => (a -> b -> m c)
    -> NonEmptyVector a
    -> NonEmptyVector b
    -> m ()
zipWithM_ f a b = V.zipWithM_ f (_neVec a) (_neVec b)
{-# INLINE zipWithM_ #-}

-- | /O(min(m,n))/ Zip the two non-empty vectors with a monadic action
-- that also takes the element index and ignore the results.
--
izipWithM_
    :: Monad m
    => (Int -> a -> b -> m c)
    -> NonEmptyVector a
    -> NonEmptyVector b
    -> m ()
izipWithM_ f a b = V.izipWithM_ f (_neVec a) (_neVec b)
{-# INLINE izipWithM_ #-}

-- ---------------------------------------------------------------------- --
-- Unzipping

-- | /O(min(m,n))/ Unzip a non-empty vector of pairs.
--
unzip :: NonEmptyVector (a, b) -> (NonEmptyVector a, NonEmptyVector b)
unzip (NonEmptyVector v) = case V.unzip v of
    ~(a,b) -> (NonEmptyVector a, NonEmptyVector b)
{-# INLINE unzip #-}

-- | Unzip a non-empty vector of triples.
--
unzip3
    :: NonEmptyVector (a, b, c)
    -> (NonEmptyVector a, NonEmptyVector b, NonEmptyVector c)
unzip3 (NonEmptyVector v) = case V.unzip3 v of
    ~(a,b,c) ->
      ( NonEmptyVector a
      , NonEmptyVector b
      , NonEmptyVector c
      )
{-# INLINE unzip3 #-}

-- | Unzip a non-empty vector of quadruples.
--
unzip4
    :: NonEmptyVector (a, b, c, d)
    -> ( NonEmptyVector a
       , NonEmptyVector b
       , NonEmptyVector c
       , NonEmptyVector d
       )
unzip4 (NonEmptyVector v) = case V.unzip4 v of
    ~(a,b,c,d) ->
      ( NonEmptyVector a
      , NonEmptyVector b
      , NonEmptyVector c
      , NonEmptyVector d
      )
{-# INLINE unzip4 #-}

-- | Unzip a non-empty vector of quintuples.
--
unzip5
    :: NonEmptyVector (a, b, c, d, e)
    -> ( NonEmptyVector a
       , NonEmptyVector b
       , NonEmptyVector c
       , NonEmptyVector d
       , NonEmptyVector e
       )
unzip5 (NonEmptyVector v) = case V.unzip5 v of
    ~(a,b,c,d,e) ->
      ( NonEmptyVector a
      , NonEmptyVector b
      , NonEmptyVector c
      , NonEmptyVector d
      , NonEmptyVector e
      )
{-# INLINE unzip5 #-}

-- | Unzip a non-empty vector of sextuples.
--
unzip6
    :: NonEmptyVector (a, b, c, d, e, f)
    -> ( NonEmptyVector a
       , NonEmptyVector b
       , NonEmptyVector c
       , NonEmptyVector d
       , NonEmptyVector e
       , NonEmptyVector f
       )
unzip6 (NonEmptyVector v) = case V.unzip6 v of
    ~(a,b,c,d,e,f) ->
      ( NonEmptyVector a
      , NonEmptyVector b
      , NonEmptyVector c
      , NonEmptyVector d
      , NonEmptyVector e
      , NonEmptyVector f
      )
{-# INLINE unzip6 #-}

-- ---------------------------------------------------------------------- --
-- Filtering

-- | /O(n)/ Drop elements that do not satisfy the predicate.
--
-- If no elements satisfy the predicate, the resulting vector may be empty.
--
filter :: (a -> Bool) -> NonEmptyVector a -> Vector a
filter f = V.filter f . _neVec
{-# INLINE filter #-}

-- | /O(n)/ Drop elements that do not satisfy the predicate which is
-- applied to values and their indices.
--
-- If no elements satisfy the predicate, the resulting vector may be empty.
--
ifilter
    :: (Int -> a -> Bool)
    -> NonEmptyVector a
    -> Vector a
ifilter f = V.ifilter f . _neVec
{-# INLINE ifilter #-}

-- | /O(n)/ Drop elements that do not satisfy the monadic predicate.
--
-- If no elements satisfy the predicate, the resulting vector may be empty.
--
filterM
    :: Monad m
    => (a -> m Bool)
    -> NonEmptyVector a
    -> m (Vector a)
filterM f = V.filterM f . _neVec
{-# INLINE filterM #-}

-- | /O(n)/ Drop repeated adjacent elements.
--
uniq :: Eq a => NonEmptyVector a -> NonEmptyVector a
uniq = NonEmptyVector . V.uniq . _neVec
{-# INLINE uniq #-}

-- | /O(n)/ Drop elements when predicate returns Nothing
--
-- If no elements satisfy the predicate, the resulting vector may be empty.
--
mapMaybe
    :: (a -> Maybe b)
    -> NonEmptyVector a
    -> Vector b
mapMaybe f = V.mapMaybe f . _neVec
{-# INLINE mapMaybe #-}

-- | /O(n)/ Drop elements when predicate, applied to index and value, returns Nothing
--
-- If no elements satisfy the predicate, the resulting vector may be empty.
--
imapMaybe
    :: (Int -> a -> Maybe b)
    -> NonEmptyVector a
    -> Vector b
imapMaybe f = V.imapMaybe f . _neVec
{-# INLINE imapMaybe #-}

-- | /O(n)/ Yield the longest prefix of elements satisfying the predicate
-- without copying.
--
-- If no elements satisfy the predicate, the resulting vector may be empty.
--
takeWhile :: (a -> Bool) -> NonEmptyVector a -> Vector a
takeWhile f = V.takeWhile f . _neVec
{-# INLINE takeWhile #-}

-- | /O(n)/ Drop the longest prefix of elements that satisfy the predicate
-- without copying.
--
-- If all elements satisfy the predicate, the resulting vector may be empty.
--
dropWhile :: (a -> Bool) -> NonEmptyVector a -> Vector a
dropWhile f = V.dropWhile f . _neVec
{-# INLINE dropWhile #-}

-- ---------------------------------------------------------------------- --
-- Partitioning

-- | /O(n)/ Split the non-empty vector in two parts, the first one
-- containing those elements that satisfy the predicate and the second
-- one those that don't. The relative order of the elements is preserved
-- at the cost of a sometimes reduced performance compared to
-- 'unstablePartition'.
--
-- If all or no elements satisfy the predicate, one of the resulting vectors
-- may be empty.
--
partition :: (a -> Bool) -> NonEmptyVector a -> (Vector a, Vector a)
partition f = V.partition f . _neVec
{-# INLINE partition #-}

-- | /O(n)/ Split the non-empty vector in two parts, the first one
-- containing those elements that satisfy the predicate and the second
-- one those that don't. The order of the elements is not preserved but
-- the operation is often faster than 'partition'.
--
-- If all or no elements satisfy the predicate, one of the resulting vectors
-- may be empty.
--
unstablePartition
    :: (a -> Bool)
    -> NonEmptyVector a
    -> (Vector a, Vector a)
unstablePartition f = V.unstablePartition f . _neVec
{-# INLINE unstablePartition #-}

-- | /O(n)/ Split the non-empty vector into the longest prefix of elements
-- that satisfy the predicate and the rest without copying.
--
-- If all or no elements satisfy the predicate, one of the resulting vectors
-- may be empty.
--
span :: (a -> Bool) -> NonEmptyVector a -> (Vector a, Vector a)
span f = V.span f . _neVec
{-# INLINE span #-}

-- | /O(n)/ Split the vector into the longest prefix of elements that do not
-- satisfy the predicate and the rest without copying.
--
-- If all or no elements satisfy the predicate, one of the resulting vectors
-- may be empty.
--
break :: (a -> Bool) -> NonEmptyVector a -> (Vector a, Vector a)
break f = V.break f . _neVec
{-# INLINE break #-}

-- ---------------------------------------------------------------------- --
-- Searching

-- | /O(n)/ Check if the non-empty vector contains an element
--
elem :: Eq a => a -> NonEmptyVector a -> Bool
elem a = V.elem a . _neVec
{-# INLINE elem #-}

-- | /O(n)/ Check if the non-empty vector does not contain an element
-- (inverse of 'elem')
--
notElem :: Eq a => a -> NonEmptyVector a -> Bool
notElem a = V.notElem a . _neVec
{-# INLINE notElem #-}

-- | /O(n)/ Yield 'Just' the first element matching the predicate or
-- 'Nothing' if no such element exists.
--
find :: (a -> Bool) -> NonEmptyVector a -> Maybe a
find f = V.find f . _neVec
{-# INLINE find #-}

-- | /O(n)/ Yield 'Just' the index of the first element matching the
-- predicate or 'Nothing' if no such element exists.
--
findIndex :: (a -> Bool) -> NonEmptyVector a -> Maybe Int
findIndex f = V.findIndex f . _neVec
{-# INLINE findIndex #-}

-- | /O(n)/ Yield the indices of elements satisfying the predicate in
-- ascending order.
--
findIndices :: (a -> Bool) -> NonEmptyVector a -> Vector Int
findIndices f = V.findIndices f . _neVec
{-# INLINE findIndices #-}

-- | /O(n)/ Yield 'Just' the index of the first occurence of the given
-- element or 'Nothing' if the non-empty vector does not contain the
-- element. This is a specialised version of 'findIndex'.
--
elemIndex :: Eq a => a -> NonEmptyVector a -> Maybe Int
elemIndex a = V.elemIndex a . _neVec
{-# INLINE elemIndex #-}

-- | /O(n)/ Yield the indices of all occurences of the given element in
-- ascending order. This is a specialised version of 'findIndices'.
--
elemIndices :: Eq a => a -> NonEmptyVector a -> Vector Int
elemIndices a = V.elemIndices a . _neVec
{-# INLINE elemIndices #-}

-- ---------------------------------------------------------------------- --
-- Folding

-- | /O(n)/ Left monoidal fold
--
foldl :: (a -> b -> a) -> a -> NonEmptyVector b -> a
foldl f a = V.foldl f a . _neVec
{-# INLINE foldl #-}

-- | /O(n)/ Left semigroupal fold
--
foldl1 :: (a -> a -> a) -> NonEmptyVector a -> a
foldl1 f = V.foldl1 f . _neVec
{-# INLINE foldl1 #-}

-- | /O(n)/ Strict Left monoidal fold
--
foldl' :: (a -> b -> a) -> a -> NonEmptyVector b -> a
foldl' f a = V.foldl' f a . _neVec
{-# INLINE foldl' #-}

-- | /O(n)/ Strict Left semigroupal fold
--
foldl1' :: (a -> a -> a) -> NonEmptyVector a -> a
foldl1' f = V.foldl1' f . _neVec
{-# INLINE foldl1' #-}

-- | /O(n)/ Right monoidal fold
--
foldr :: (a -> b -> b) -> b -> NonEmptyVector a -> b
foldr f b = V.foldr f b . _neVec
{-# INLINE foldr #-}

-- | /O(n)/ Right semigroupal fold
--
foldr1 :: (a -> a -> a) -> NonEmptyVector a -> a
foldr1 f = V.foldr1 f . _neVec
{-# INLINE foldr1 #-}

-- | /O(n)/ Strict right monoidal fold
--
foldr' :: (a -> b -> b) -> b -> NonEmptyVector a -> b
foldr' f b = V.foldr' f b. _neVec
{-# INLINE foldr' #-}

-- | /O(n)/ Strict right semigroupal fold
--
foldr1' :: (a -> a -> a) -> NonEmptyVector a -> a
foldr1' f = V.foldr1' f . _neVec
{-# INLINE foldr1' #-}

-- | /O(n)/ Left monoidal fold with function applied to each element
-- and its index
--
ifoldl :: (a -> Int -> b -> a) -> a -> NonEmptyVector b -> a
ifoldl f a = V.ifoldl f a . _neVec
{-# INLINE ifoldl #-}

-- | /O(n)/ Strict left monoidal fold with function applied to each element
-- and its index
--
ifoldl' :: (a -> Int -> b -> a) -> a -> NonEmptyVector b -> a
ifoldl' f a = V.ifoldl' f a . _neVec
{-# INLINE ifoldl' #-}

-- | /O(n)/ Right monoidal fold with function applied to each element
-- and its index
--
ifoldr :: (Int -> a -> b -> b) -> b -> NonEmptyVector a -> b
ifoldr f b = V.ifoldr f b . _neVec
{-# INLINE ifoldr #-}

-- | /O(n)/ strict right monoidal fold with function applied to each element
-- and its index
--
ifoldr' :: (Int -> a -> b -> b) -> b -> NonEmptyVector a -> b
ifoldr' f b = V.ifoldr' f b . _neVec
{-# INLINE ifoldr' #-}

-- ---------------------------------------------------------------------- --
-- Specialised folds

-- | /O(n)/ Check if all elements satisfy the predicate.
--
all :: (a -> Bool) -> NonEmptyVector a -> Bool
all f = V.all f . _neVec
{-# INLINE all #-}

-- | /O(n)/ Check if any element satisfies the predicate.
--
any :: (a -> Bool) -> NonEmptyVector a -> Bool
any f = V.any f . _neVec
{-# INLINE any #-}

-- | /O(n)/ Check if all elements are @True@.
--
and :: NonEmptyVector Bool -> Bool
and = V.and . _neVec
{-# INLINE and #-}

-- | /O(n)/ Check if any element is @True@
--
or :: NonEmptyVector Bool -> Bool
or = V.or . _neVec
{-# INLINE or #-}

-- | /O(n)/ Compute the sum of the elements
--
sum :: Num a => NonEmptyVector a -> a
sum = V.sum . _neVec
{-# INLINE sum #-}

-- | /O(n)/ Compute the produce of the elements
--
product :: Num a => NonEmptyVector a -> a
product = V.product . _neVec
{-# INLINE product #-}

-- | /O(n)/ Yield the maximum element of the non-empty vector.
--
maximum :: Ord a => NonEmptyVector a -> a
maximum = V.maximum . _neVec
{-# INLINE maximum #-}

-- | /O(n)/ Yield the maximum element of a non-empty vector
-- according to the given comparison function.
--
maximumBy :: (a -> a -> Ordering) -> NonEmptyVector a -> a
maximumBy f = V.maximumBy f . _neVec
{-# INLINE maximumBy #-}

-- | /O(n)/ Yield the minimum element of the non-empty vector.
--
minimum :: Ord a => NonEmptyVector a -> a
minimum = V.minimum . _neVec
{-# INLINE minimum #-}

-- | /O(n)/ Yield the minimum element of the non-empty vector
-- according to the given comparison function.
--
minimumBy :: (a -> a -> Ordering) -> NonEmptyVector a -> a
minimumBy f = V.minimumBy f . _neVec
{-# INLINE minimumBy #-}

-- | /O(n)/ Yield the index of the minimum element of the
-- non-empty vector.
--
minIndex :: Ord a => NonEmptyVector a -> Int
minIndex = V.minIndex . _neVec
{-# INLINE minIndex #-}

-- | /O(n)/ Yield the index of the minimum element of the vector
-- according to the given comparison function.
--
minIndexBy :: (a -> a -> Ordering) -> NonEmptyVector a -> Int
minIndexBy f = V.minIndexBy f . _neVec
{-# INLINE minIndexBy #-}

-- | /O(n)/ Yield the index of the maximum element of the
-- non-empty vector.
--
maxIndex :: Ord a => NonEmptyVector a -> Int
maxIndex = V.maxIndex . _neVec
{-# INLINE maxIndex #-}

-- | /O(n)/ Yield the index of the maximum element of the vector
-- according to the given comparison function.
--
maxIndexBy :: (a -> a -> Ordering) -> NonEmptyVector a -> Int
maxIndexBy f = V.maxIndexBy f . _neVec
{-# INLINE maxIndexBy #-}

-- ---------------------------------------------------------------------- --
-- Monadic folds

-- | /O(n)/ Monadic fold
--
foldM :: Monad m => (a -> b -> m a) -> a -> NonEmptyVector b -> m a
foldM f a = V.foldM f a . _neVec
{-# INLINE foldM #-}

-- | /O(n)/ Monadic fold (action applied to each element and its index)
--
ifoldM :: Monad m => (a -> Int -> b -> m a) -> a -> NonEmptyVector b -> m a
ifoldM f a = V.ifoldM f a . _neVec
{-# INLINE ifoldM #-}

-- | /O(n)/ Strict monadic fold
--
foldM' :: Monad m => (a -> b -> m a) -> a -> NonEmptyVector b -> m a
foldM' f a = V.foldM' f a . _neVec
{-# INLINE foldM' #-}

-- | /O(n)/ Strict monadic fold (action applied to each element and its index)
--
ifoldM' :: Monad m => (a -> Int -> b -> m a) -> a -> NonEmptyVector b -> m a
ifoldM' f a = V.ifoldM' f a . _neVec
{-# INLINE ifoldM' #-}

-- | /O(n)/ Monadic semigroupal fold
--
fold1M :: Monad m => (a -> a -> m a) -> NonEmptyVector a -> m a
fold1M f = V.fold1M f . _neVec
{-# INLINE fold1M #-}

-- | /O(n)/ Strict monadic semigroupal fold
--
fold1M' :: Monad m => (a -> a -> m a) -> NonEmptyVector a -> m a
fold1M' f = V.fold1M' f . _neVec
{-# INLINE fold1M' #-}

-- | /O(n)/ Monadic fold that discards the result
--
foldM_ :: Monad m => (a -> b -> m a) -> a -> NonEmptyVector b -> m ()
foldM_ f a = V.foldM_ f a . _neVec
{-# INLINE foldM_ #-}

-- | /O(n)/ Monadic fold that discards the result (action applied to each
-- element and its index)
--
ifoldM_ :: Monad m => (a -> Int -> b -> m a) -> a -> NonEmptyVector b -> m ()
ifoldM_ f a = V.ifoldM_ f a . _neVec
{-# INLINE ifoldM_ #-}

-- | /O(n)/ Strict monadic fold that discards the result
--
foldM'_ :: Monad m => (a -> b -> m a) -> a -> NonEmptyVector b -> m ()
foldM'_ f a = V.foldM'_ f a . _neVec
{-# INLINE foldM'_ #-}

-- | /O(n)/ Strict monadic fold that discards the result (action applied to each
-- element and its index)
--
ifoldM'_ :: Monad m => (a -> Int -> b -> m a) -> a -> NonEmptyVector b -> m ()
ifoldM'_ f a = V.ifoldM'_ f a . _neVec
{-# INLINE ifoldM'_ #-}

-- | /O(n)/ Monadic semigroupal fold that discards the result
--
fold1M_ :: Monad m => (a -> a -> m a) -> NonEmptyVector a -> m ()
fold1M_ f = V.fold1M_ f . _neVec
{-# INLINE fold1M_ #-}

-- | /O(n)/ Strict monadic semigroupal fold that discards the result
--
fold1M'_ :: Monad m => (a -> a -> m a) -> NonEmptyVector a -> m ()
fold1M'_ f = V.fold1M'_ f . _neVec
{-# INLINE fold1M'_ #-}

-- ---------------------------------------------------------------------- --
-- Monadic sequencing

-- | Evaluate each action and collect the results
--
sequence :: Monad m => NonEmptyVector (m a) -> m (NonEmptyVector a)
sequence = fmap NonEmptyVector . V.sequence . _neVec
{-# INLINE sequence #-}

-- | Evaluate each action and discard the results
--
sequence_ :: Monad m => NonEmptyVector (m a) -> m ()
sequence_ = V.sequence_ . _neVec
{-# INLINE sequence_ #-}

-- ---------------------------------------------------------------------- --
-- Prefix sums (scans)

-- | /O(n)/ Prescan
--
prescanl :: (a -> b -> a) -> a -> NonEmptyVector b -> NonEmptyVector a
prescanl f a = NonEmptyVector . V.prescanl f a . _neVec
{-# INLINE prescanl #-}

-- | /O(n)/ Prescan with strict accumulator
--
prescanl' :: (a -> b -> a) -> a -> NonEmptyVector b -> NonEmptyVector a
prescanl' f a = NonEmptyVector . V.prescanl' f a . _neVec
{-# INLINE prescanl' #-}

-- | /O(n)/ Scan
--
postscanl :: (a -> b -> a) -> a -> NonEmptyVector b -> NonEmptyVector a
postscanl f a = NonEmptyVector . V.postscanl f a . _neVec
{-# INLINE postscanl #-}

-- | /O(n)/ Scan with a strict accumulator
--
postscanl' :: (a -> b -> a) -> a -> NonEmptyVector b -> NonEmptyVector a
postscanl' f a = NonEmptyVector . V.postscanl' f a . _neVec
{-# INLINE postscanl' #-}

-- | /O(n)/ Haskell-style scan
--
scanl :: (a -> b -> a) -> a -> NonEmptyVector b -> NonEmptyVector a
scanl f a = NonEmptyVector . V.scanl f a . _neVec
{-# INLINE scanl #-}

-- | /O(n)/ Haskell-style scan with strict accumulator
--
scanl' :: (a -> b -> a) -> a -> NonEmptyVector b -> NonEmptyVector a
scanl' f a = NonEmptyVector . V.scanl' f a . _neVec
{-# INLINE scanl' #-}

-- | /O(n)/ Semigroupal left scan
--
scanl1 :: (a -> a -> a) -> NonEmptyVector a -> NonEmptyVector a
scanl1 f = NonEmptyVector . V.scanl1 f . _neVec
{-# INLINE scanl1 #-}

-- | /O(n)/ Strict semigroupal scan
--
scanl1' :: (a -> a -> a) -> NonEmptyVector a -> NonEmptyVector a
scanl1' f = NonEmptyVector . V.scanl1' f . _neVec
{-# INLINE scanl1' #-}

-- | /O(n)/ Scan over a vector with its index
--
iscanl :: (Int -> a -> b -> a) -> a -> NonEmptyVector b -> NonEmptyVector a
iscanl f a = NonEmptyVector . V.iscanl f a . _neVec
{-# INLINE iscanl #-}

-- | /O(n)/ Scan over a vector with its index with strict accumulator
--
iscanl' :: (Int -> a -> b -> a) -> a -> NonEmptyVector b -> NonEmptyVector a
iscanl' f a = NonEmptyVector . V.iscanl' f a . _neVec
{-# INLINE iscanl' #-}

-- | /O(n)/ Right-to-left prescan
--
prescanr :: (a -> b -> b) -> b -> NonEmptyVector a -> NonEmptyVector b
prescanr f b = NonEmptyVector . V.prescanr f b . _neVec
{-# INLINE prescanr #-}

-- | /O(n)/ Right-to-left prescan with strict accumulator
--
prescanr' :: (a -> b -> b) -> b -> NonEmptyVector a -> NonEmptyVector b
prescanr' f b = NonEmptyVector . V.prescanr f b . _neVec
{-# INLINE prescanr' #-}

-- | /O(n)/ Right-to-left scan
--
postscanr :: (a -> b -> b) -> b -> NonEmptyVector a -> NonEmptyVector b
postscanr f b = NonEmptyVector . V.postscanr f b . _neVec
{-# INLINE postscanr #-}

-- | /O(n)/ Right-to-left scan with strict accumulator
--
postscanr' :: (a -> b -> b) -> b -> NonEmptyVector a -> NonEmptyVector b
postscanr' f b = NonEmptyVector . V.postscanr' f b . _neVec
{-# INLINE postscanr' #-}

-- | /O(n)/ Right-to-left Haskell-style scan
--
scanr :: (a -> b -> b) -> b -> NonEmptyVector a -> NonEmptyVector b
scanr f b = NonEmptyVector . V.scanr f b . _neVec
{-# INLINE scanr #-}

-- | /O(n)/ Right-to-left Haskell-style scan with strict accumulator
--
scanr' :: (a -> b -> b) -> b -> NonEmptyVector a -> NonEmptyVector b
scanr' f b = NonEmptyVector . V.scanr' f b . _neVec
{-# INLINE scanr' #-}

-- | /O(n)/ Right-to-left Haskell-style semigroupal scan
--
scanr1 :: (a -> a -> a) -> NonEmptyVector a -> NonEmptyVector a
scanr1 f = NonEmptyVector . V.scanr1 f . _neVec
{-# INLINE scanr1 #-}

-- | /O(n)/ Right-to-left Haskell-style semigroupal scan with strict accumulator
--
scanr1' :: (a -> a -> a) -> NonEmptyVector a -> NonEmptyVector a
scanr1' f = NonEmptyVector . V.scanr1' f . _neVec
{-# INLINE scanr1' #-}

-- | /O(n)/ Right-to-left scan over a vector with its index
--
iscanr :: (Int -> a -> b -> b) -> b -> NonEmptyVector a -> NonEmptyVector b
iscanr f b = NonEmptyVector . V.iscanr f b . _neVec
{-# INLINE iscanr #-}

-- | /O(n)/ Right-to-left scan over a vector with its index and a strict
-- accumulator
--
iscanr' :: (Int -> a -> b -> b) -> b -> NonEmptyVector a -> NonEmptyVector b
iscanr' f b = NonEmptyVector . V.iscanr' f b . _neVec
{-# INLINE iscanr' #-}
