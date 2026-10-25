{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
-- |
-- Module       : Data.Vector.NonEmptyU
-- Copyright 	: (c) 2019-2026 Emily Pillmore, Koz Ross
-- License	: BSD-style
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : DataTypeable, CPP
--
-- A library for non-empty unboxed vectors (that is, polymorphic arrays capable of
-- holding any value whose type has an 'V.Unbox' isntance). Non-empty vectors come in 
-- two flavors:
--
--  * mutable
--
--  * immutable
--
-- This library attempts to provide support for all standard 'Vector' operations
-- in the API, with some slight variation in types and implementation. For example,
-- since 'head' and 'foldr' are always guaranteed to be over a non-empty 'Vector',
-- it is safe to make use of the 'unsafe-*' 'Vector' operations and semigroupal
-- folds available in the API in lieu of the standard implementations.
--
-- In contrast, some operations such as 'filter' may "break out" of a t'NonEmptyUVector'
-- due to the fact that there are no guarantees that may be made on the types of
-- 'Bool'-valued functions passed in, hence one could write the following:
--
-- @
-- filter (const false) v
-- @
--
-- which always produces an empty vector. Thus, some operations must return either
-- a 'Maybe' containing a t'NonEmptyUVector' or a 'Vector' whenever appropriate. Generally
-- The former is used in initialization and generation operations, and the latter
-- is used in iterative operations where the intent is not to create an instance
-- of t'NonEmptyUVector'.
--
-- Credit to Roman Leshchinskiy for the original Vector library  upon which this is based.
--
module Data.Vector.NonEmptyU
( -- * Boxed non-empty vectors
  NonEmptyUVector

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
, enumFromN, enumFromN1
, enumFromStepN, enumFromStepN1
, enumFromTo, enumFromThenTo

  -- ** Concatenation
, cons, consV, snoc, snocV, (++), concat, concat1

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
, uniq, mapMaybe, imapMaybe
, filter, ifilter, filterM, ifilterM
, takeWhile, dropWhile

  -- * Partitioning
, partition, partitionWith, unstablePartition, span, break

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

  -- * Prefix sums (scans)
, prescanl, prescanl', postscanl, postscanl'
, scanl, scanl', scanl1, scanl1', iscanl, iscanl'
, prescanr, prescanr', postscanr, postscanr'
, scanr, scanr', scanr1, scanr1', iscanr, iscanr'

  -- ** Transformations
, intersperse

  -- ** Grouping
, group
, groupBy

) where


import Prelude ( Bool, Eq, Ord, Num, Enum
               , (.), Ordering, max, uncurry, snd
               , pure, (+), otherwise, (>=), (-), (*), ($), (<=)
               , (==), not)

import Control.Monad (Monad)
import Control.Monad.ST

import qualified Data.Foldable as Foldable
import Data.Either (Either(..))
#if MIN_VERSION_base(4,19,0)
-- This import will only execute if compiling with base >= 4.19.0.0 (GHC 9.8+)
import Data.Functor hiding (unzip)
#else
import Data.Functor
#endif
import Data.Int
import Data.List.NonEmpty (NonEmpty(..), (<|))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Kind (Type)
import Data.Maybe (Maybe(..))
import Data.Semigroup (Semigroup(..), (<>))
import Data.Traversable (Traversable)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Generic as G
import Data.Vector.Unboxed.Mutable (MVector, unsafeWrite)
import Data.Vector.NonEmpty.InternalU


-- $setup
-- >>> import Prelude (Int, String, ($), (.), (+), (<), const, return, Maybe(..), Either(..))
-- >>> import Data.Bool
-- >>> import Data.Eq
-- >>> import qualified Prelude as P
-- >>> import qualified Data.Vector.Unboxed as V
-- >>> import Data.List.NonEmpty (NonEmpty(..))
-- >>> import qualified Data.List.NonEmpty as NEL
-- >>> :set -XTypeApplications
-- >>> :set -XScopedTypeVariables

-- ---------------------------------------------------------------------- --
-- Accessors + Indexing

-- | /O(1)/ Length.
--
-- >>> length $ unsafeFromList [1..10]
-- 10
--
length :: V.Unbox a => NonEmptyUVector a -> Int
length = V.length . _neUvec
{-# INLINE length #-}

-- | /O(1)/ First element. Since head is guaranteed, bounds checks
-- are bypassed by deferring to 'Data.Vector.unsafeHead'.
--
--
-- >>> head $ unsafeFromList [1..10]
-- 1
--
head :: V.Unbox a => NonEmptyUVector a -> a
head = V.unsafeHead . _neUvec
{-# INLINE head #-}

-- | /O(1)/ Last element. Since a last element is guaranteed, bounds checks
-- are bypassed by deferring to 'Data.Vector.unsafeLast'.
--
--
-- >>> last $ unsafeFromList [1..10]
-- 10
--
last :: V.Unbox a => NonEmptyUVector a -> a
last = V.unsafeLast . _neUvec
{-# INLINE last #-}

-- | /O(1)/ Indexing.
--
--
-- >>> (unsafeFromList [1..10]) ! 0
-- 1
--
(!) :: V.Unbox a => NonEmptyUVector a -> Int -> a
(!) (NonEmptyUVector as) n = as V.! n
{-# INLINE (!) #-}

-- | /O(1)/ Safe indexing.
--
--
-- >>> (unsafeFromList [1..10]) !? 0
-- Just 1
--
-- >>> (unsafeFromList [1..10]) !? 11
-- Nothing
--
(!?) :: V.Unbox a => NonEmptyUVector a -> Int -> Maybe a
(NonEmptyUVector as) !? n = as V.!? n
{-# INLINE (!?) #-}

-- | /O(1)/ Unsafe indexing without bounds checking
--
unsafeIndex :: V.Unbox a => NonEmptyUVector a -> Int -> a
unsafeIndex (NonEmptyUVector as) n = V.unsafeIndex as n
{-# INLINE unsafeIndex #-}

-- ---------------------------------------------------------------------- --
-- Monadic Indexing

-- | /O(1)/ Indexing in a monad.
--
-- The monad allows operations to be strict in the non-empty vector when
-- necessary.
--
-- See 'Data.Vector.indexM' for more details
--
--
-- >>> indexM @[] (unsafeFromList [1..10]) 3
-- [4]
--
indexM :: (Monad m, V.Unbox a) => NonEmptyUVector a -> Int -> m a
indexM (NonEmptyUVector v) n = V.indexM v n
{-# INLINE indexM #-}

-- | /O(1)/ First element of a non-empty vector in a monad.
--
-- See 'Data.Vector.indexM' for an explanation of why this is useful.
--
-- Note that this function defers to 'Data.Vector.unsafeHeadM' since head is
-- guaranteed to be safe by construction.
--
--
-- >>> headM @[] (unsafeFromList [1..10])
-- [1]
--
headM :: (Monad m, V.Unbox a) => NonEmptyUVector a -> m a
headM (NonEmptyUVector v) = V.unsafeHeadM v
{-# INLINE headM #-}

-- | /O(1)/ Last element of a non-empty vector in a monad. See 'Data.Vector.indexM'
-- for an explanation of why this is useful.
--
-- Note that this function defers to 'Data.Vector.unsafeLastM' since a last element is
-- guaranteed.
--
--
-- >>> lastM @[] (unsafeFromList [1..10])
-- [10]
--
lastM :: (Monad m, V.Unbox a) => NonEmptyUVector a -> m a
lastM (NonEmptyUVector v) = V.unsafeLastM v
{-# INLINE lastM #-}

-- | O(1) Indexing in a monad without bounds checks. See 'indexM' for an
-- explanation of why this is useful.
--
unsafeIndexM :: (Monad m, V.Unbox a) => NonEmptyUVector a -> Int -> m a
unsafeIndexM (NonEmptyUVector v) n = V.unsafeIndexM v n
{-# INLINE unsafeIndexM #-}

-- ---------------------------------------------------------------------- --
-- Extracting subvectors (slicing)

-- | /O(1)/ Yield all but the first element without copying. Since the
-- vector returned may be empty (i.e. input was a singleton), this function
-- returns a normal 'Vector'
--
--
-- >>> tail (unsafeFromList [1..10])
-- [2,3,4,5,6,7,8,9,10]
--
tail :: V.Unbox a => NonEmptyUVector a -> Vector a
tail = V.unsafeTail . _neUvec
{-# INLINE tail #-}

-- | /O(1)/ Yield a slice of a non-empty vector without copying at
-- the @0@th and @1@st indices.
--
--
-- >>> uncons (unsafeFromList [1..10])
-- (1,[2,3,4,5,6,7,8,9,10])
--
uncons :: V.Unbox a => NonEmptyUVector a -> (a, Vector a)
uncons v = (head v, tail v)
{-# INLINE uncons #-}

-- | /O(1)/ Yield a slice of a non-empty vector without copying at
-- the @n-1@th and @nth@ indices
--
--
-- >>> unsnoc (unsafeFromList [1..10])
-- ([1,2,3,4,5,6,7,8,9],10)
--
unsnoc :: V.Unbox a => NonEmptyUVector a -> (Vector a, a)
unsnoc v = (init v, last v)
{-# INLINE unsnoc #-}

-- | /O(1)/ Yield a slice of the non-empty vector without copying it.
-- The vector must contain at least i+n elements. Because this is not
-- guaranteed, this function returns a 'Vector' which could be empty
--
--
-- >>> slice 0 3 (unsafeFromList [1..10])
-- [1,2,3]
--
slice :: V.Unbox a => Int -> Int -> NonEmptyUVector a -> Vector a
slice i n = V.slice i n . _neUvec
{-# INLINE slice #-}

-- | /O(1)/ Yield all but the last element without copying. Since the
-- vector returned may be empty (i.e. input was a singleton), this function
-- returns a normal 'Vector'
--
--
-- >>> init (unsafeFromList [1..3])
-- [1,2]
--
init :: V.Unbox a => NonEmptyUVector a -> Vector a
init = V.unsafeInit . _neUvec
{-# INLINE init #-}

-- | /O(1)/ Yield at the first n elements without copying. The non-empty vector may
-- contain less than n elements in which case it is returned as a vector unchanged.
--
--
-- >>> take 2 (unsafeFromList [1..3])
-- [1,2]
--
take :: V.Unbox a => Int -> NonEmptyUVector a -> Vector a
take n = V.take n . _neUvec
{-# INLINE take #-}

-- | /O(1)/ Yield all but the first n elements without copying. The non-empty vector
-- may contain less than n elements in which case an empty vector is returned.
--
--
-- >>> drop 2 (unsafeFromList [1..3])
-- [3]
--
drop :: V.Unbox a => Int -> NonEmptyUVector a -> Vector a
drop n = V.drop n . _neUvec
{-# INLINE drop #-}

-- | /O(1)/ Yield the first n elements paired with the remainder without copying.
--
-- This function returns a pair of vectors, as one may slice a (0, n+1).
--
--
-- >>> splitAt 2 (unsafeFromList [1..3])
-- ([1,2],[3])
--
splitAt :: V.Unbox a => Int -> NonEmptyUVector a -> (Vector a, Vector a)
splitAt n = V.splitAt n . _neUvec
{-# INLINE splitAt #-}

-- | /O(1)/ Yield a slice of the vector without copying. The vector must contain at
-- least i+n elements but this is not checked.
--
unsafeSlice :: V.Unbox a => Int -> Int -> NonEmptyUVector a -> Vector a
unsafeSlice i n = V.unsafeSlice i n . _neUvec
{-# INLINE unsafeSlice #-}

-- | /O(1)/ Yield the first n elements without copying. The vector must contain at
-- least n elements but this is not checked.
--
unsafeTake :: V.Unbox a => Int -> NonEmptyUVector a -> Vector a
unsafeTake n = V.unsafeTake n . _neUvec
{-# INLINE unsafeTake #-}

-- | /O(1)/ Yield all but the first n elements without copying. The vector must contain
-- at least n elements but this is not checked.
--
unsafeDrop :: V.Unbox a => Int -> NonEmptyUVector a -> Vector a
unsafeDrop n = V.unsafeDrop n . _neUvec
{-# INLINE unsafeDrop #-}

-- ---------------------------------------------------------------------- --
-- Construction

-- | /O(1)/ Non-empty vector with exactly one element
--
--
-- >>> singleton "a"
-- ["a"]
--
singleton :: V.Unbox a => a -> NonEmptyUVector a
singleton = NonEmptyUVector . V.singleton
{-# INLINE singleton #-}

-- | /O(n)/ Non-empty vector of the given length with the same value in
-- each position.
--
-- When given a index n <= 0, then 'Nothing' is returned, otherwise 'Just'.
--
--
-- >>> replicate 3 "a"
-- Just ["a","a","a"]
--
-- >>> replicate 0 "a"
-- Nothing
--
replicate :: V.Unbox a => Int -> a -> Maybe (NonEmptyUVector a)
replicate n a = fromVector (V.replicate n a)
{-# INLINE replicate #-}

-- | /O(n)/ Non-empty vector of the given length with the same value in
-- each position.
--
-- This variant takes @max n 1@ for the supplied length parameter.
--
--
-- >>> replicate1 3 "a"
-- ["a","a","a"]
--
-- >>> replicate1 0 "a"
-- ["a"]
--
-- >>> replicate1 (-1) "a"
-- ["a"]
--
replicate1 :: V.Unbox a => Int -> a -> NonEmptyUVector a
replicate1 n a = unsafeFromVector (V.replicate (max n 1) a)
{-# INLINE replicate1 #-}

-- | /O(n)/ Construct a vector of the given length by applying the function to
-- each index.
--
-- When given a index n <= 0, then 'Nothing' is returned, otherwise 'Just'.
--
--
-- >>> let f 0 = "a"; f _ = "k"; f :: Int -> String
--
-- >>> generate 1 f
-- Just ["a"]
--
-- >>> generate 0 f
-- Nothing
--
-- >>> generate 2 f
-- Just ["a","k"]
--
generate :: V.Unbox a => Int -> (Int -> a) -> Maybe (NonEmptyUVector a)
generate n f = fromVector (V.generate n f)
{-# INLINE generate #-}

-- | /O(n)/ Construct a vector of the given length by applying the function to
-- each index.
--
-- This variant takes @max n 1@ for the supplied length parameter.
--
--
-- >>> let f 0 = "a"; f _ = "k"; f :: Int -> String
--
-- >>> generate1 2 f
-- ["a","k"]
--
-- >>> generate1 0 f
-- ["a"]
--
-- >>> generate1 (-1) f
-- ["a"]
--
generate1 :: V.Unbox a => Int -> (Int -> a) -> NonEmptyUVector a
generate1 n f = unsafeFromVector (V.generate (max n 1) f)
{-# INLINE generate1 #-}

-- | /O(n)/ Apply function n times to value. Zeroth element is original value.
--
-- When given a index n <= 0, then 'Nothing' is returned, otherwise 'Just'.
--
-- >>> iterateN 3 (+1) 0
-- Just [0,1,2]
--
-- >>> iterateN 0 (+1) 0
-- Nothing
--
-- >>> iterateN (-1) (+1) 0
-- Nothing
--
iterateN :: V.Unbox a => Int -> (a -> a) -> a -> Maybe (NonEmptyUVector a)
iterateN n f a = fromVector (V.iterateN n f a)
{-# INLINE iterateN #-}

-- | /O(n)/ Apply function n times to value. Zeroth element is original value.
--
-- This variant takes @max n 1@ for the supplied length parameter.
--
--
-- >>> iterateN1 3 (+1) 0
-- [0,1,2]
--
-- >>> iterateN1 0 (+1) 0
-- [0]
--
-- >>> iterateN1 (-1) (+1) 0
-- [0]
--
iterateN1 :: V.Unbox a => Int -> (a -> a) -> a -> NonEmptyUVector a
iterateN1 n f a = unsafeFromVector (V.iterateN (max n 1) f a)
{-# INLINE iterateN1 #-}

-- ---------------------------------------------------------------------- --
-- Monadic Initialization

-- | /O(n)/ Execute the monadic action the given number of times and store
-- the results in a vector.
--
-- When given a index n <= 0, then 'Nothing' is returned, otherwise 'Just'.
--
--
-- >>> replicateM @Maybe 3 (Just "a")
-- Just (Just ["a","a","a"])
--
-- >>> replicateM @Maybe 3 Nothing
-- Nothing
--
-- >>> replicateM @Maybe 0 (Just "a")
-- Just Nothing
--
-- >>> replicateM @Maybe (-1) (Just "a")
-- Just Nothing
--
replicateM :: (Monad m, V.Unbox a) => Int -> m a -> m (Maybe (NonEmptyUVector a))
replicateM n a = fmap fromVector (V.replicateM n a)
{-# INLINE replicateM #-}

-- | /O(n)/ Execute the monadic action the given number of times and store
-- the results in a vector.
--
-- This variant takes @max n 1@ for the supplied length parameter.
--
--
-- >>> replicate1M @Maybe 3 (Just "a")
-- Just ["a","a","a"]
--
-- >>> replicate1M @Maybe 3 Nothing
-- Nothing
--
-- >>> replicate1M @Maybe 0 (Just "a")
-- Just ["a"]
--
-- >>> replicate1M @Maybe (-1) (Just "a")
-- Just ["a"]
--
replicate1M :: (Monad m, V.Unbox a) => Int -> m a -> m (NonEmptyUVector a)
replicate1M n a = fmap unsafeFromVector (V.replicateM (max n 1) a)
{-# INLINE replicate1M #-}

-- | /O(n)/ Construct a vector of the given length by applying the monadic
-- action to each index
--
-- When given a index n <= 0, then 'Nothing' is returned, otherwise 'Just'.
--
-- >>> generateM 3 (\i -> if i P.< 1 then ["a"] else ["b"])
-- [Just ["a","b","b"]]
--
-- >>> generateM @[] @Int 3 (const [])
-- []
--
-- >>> generateM @[] @Int 0 (const [1])
-- [Nothing]
--
-- >>> generateM @Maybe @Int (-1) (const Nothing)
-- Just Nothing
--
generateM :: (Monad m, V.Unbox a) => Int -> (Int -> m a) -> m (Maybe (NonEmptyUVector a))
generateM n f = fmap fromVector (V.generateM n f)
{-# INLINE generateM #-}

-- | /O(n)/ Construct a vector of the given length by applying the monadic
-- action to each index
--
-- This variant takes @max n 1@ for the supplied length parameter.
--
-- >>> generate1M 3 (\i -> if i P.< 1 then Just "a" else Just "b")
-- Just ["a","b","b"]
--
-- >>> generate1M 3 (const [])
-- []
--
-- >>> generate1M 0 (const $ Just 1)
-- Just [1]
--
-- >>> generate1M (-1) (const Nothing)
-- Nothing
--
generate1M :: (Monad m, V.Unbox a) => Int -> (Int -> m a) -> m (NonEmptyUVector a)
generate1M n f = fmap unsafeFromVector (V.generateM (max n 1) f)
{-# INLINE generate1M #-}

-- | /O(n)/ Apply monadic function n times to value. Zeroth element is
-- original value.
--
-- When given a index n <= 0, then 'Nothing' is returned, otherwise 'Just'.
--
--
-- >>> iterateNM @Maybe 3 return "a"
-- Just (Just ["a","a","a"])
--
-- >>> iterateNM @Maybe 3 (const Nothing) "a"
-- Nothing
--
-- >>> iterateNM @Maybe 0 return "a"
-- Just Nothing
--
iterateNM :: (Monad m, V.Unbox a) => Int -> (a -> m a) -> a -> m (Maybe (NonEmptyUVector a))
iterateNM n f a = fmap fromVector (V.iterateNM n f a)
{-# INLINE iterateNM #-}

-- | /O(n)/ Apply monadic function n times to value. Zeroth element is
-- original value.
--
-- This variant takes @max n 1@ for the supplied length parameter.
--
--
-- >>> iterateN1M @Maybe 3 return "a"
-- Just ["a","a","a"]
--
-- >>> iterateN1M @Maybe 3 (const Nothing) "a"
-- Nothing
--
-- >>> iterateN1M @Maybe 0 return "a"
-- Just ["a"]
--
-- >>> iterateN1M @Maybe (-1) return "a"
-- Just ["a"]
--
iterateN1M :: (Monad m, V.Unbox a) => Int -> (a -> m a) -> a -> m (NonEmptyUVector a)
iterateN1M n f a = fmap unsafeFromVector (V.iterateNM (max n 1) f a)
{-# INLINE iterateN1M #-}

-- | Execute the monadic action and freeze the resulting non-empty vector.
--
create :: V.Unbox a => (forall s. ST s (MVector s a)) -> Maybe (NonEmptyUVector a)
create p = fromVector (G.create p)
{-# INLINE create #-}

-- | Execute the monadic action and freeze the resulting non-empty vector,
-- bypassing emptiness checks.
--
-- The onus is on the caller to guarantee the created vector is non-empty.
--
unsafeCreate :: V.Unbox a => (forall s. ST s (MVector s a)) -> NonEmptyUVector a
unsafeCreate p = unsafeFromVector (G.create p)
{-# INLINE unsafeCreate #-}

-- | Execute the monadic action and freeze the resulting non-empty vector.
--
createT
    :: (Traversable t, V.Unbox a)
    => (forall s. ST s (t (MVector s a)))
    -> t (Maybe (NonEmptyUVector a))
createT p = fmap fromVector (G.createT p)
{-# INLINE createT #-}

-- | Execute the monadic action and freeze the resulting non-empty vector.
--
-- The onus is on the caller to guarantee the created vector is non-empty.
--
unsafeCreateT
    :: (Traversable t, V.Unbox a)
    => (forall s. ST s (t (MVector s a)))
    -> t (NonEmptyUVector a)
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
--
-- >>> unfoldr (\b -> case b of "a" -> Just ("a", "b"); _ ->  Nothing) "a"
-- Just ["a"]
--
-- >>> unfoldr (const Nothing) "a"
-- Nothing
--
unfoldr :: V.Unbox a => (b -> Maybe (a, b)) -> b -> Maybe (NonEmptyUVector a)
unfoldr f b = fromVector (V.unfoldr f b)
{-# INLINE unfoldr #-}

-- | /O(n)/ Construct a non-empty vector by repeatedly applying the
-- generator function to a seed and a first element.
--
-- This variant of 'unfoldr' guarantees the resulting vector is non-
-- empty by supplying an initial element @a@.
--
--
-- >>> unfoldr1 (\b -> case b of "a" -> Just ("a", "b"); _ ->  Nothing) "first" "a"
-- ["first","a"]
--
-- >>> unfoldr1 (const Nothing) "first" "a"
-- ["first"]
--
unfoldr1 :: V.Unbox a => (b -> Maybe (a, b)) -> a -> b -> NonEmptyUVector a
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
--
-- >>> unfoldrN 3 (\b -> Just (b+1, b+1)) 0
-- Just [1,2,3]
--
-- >>> unfoldrN 3 (const Nothing) 0
-- Nothing
--
-- >>> unfoldrN 0 (\b -> Just (b+1, b+1)) 0
-- Nothing
--
unfoldrN :: V.Unbox a => Int -> (b -> Maybe (a, b)) -> b -> Maybe (NonEmptyUVector a)
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
--
-- >>> unfoldr1N 3 (\b -> Just (b+1, b+1)) 0 0
-- [0,1,2,3]
--
-- >>> unfoldr1N 3 (const Nothing) 0 0
-- [0]
--
-- >>> unfoldr1N 0 (\b -> Just (b+1, b+1)) 0 0
-- [0]
--
unfoldr1N
    :: V.Unbox a
    => Int
    -> (b -> Maybe (a, b))
    -> a
    -> b
    -> NonEmptyUVector a
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
    :: (V.Unbox a, Monad m)
    => (b -> m (Maybe (a, b)))
    -> b
    -> m (Maybe (NonEmptyUVector a))
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
    :: (Monad m, V.Unbox a)
    => (b -> m (Maybe (a, b)))
    -> a
    -> b
    -> m (NonEmptyUVector a)
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
    :: (V.Unbox a, Monad m)
    => Int
    -> (b -> m (Maybe (a, b)))
    -> b
    -> m (Maybe (NonEmptyUVector a))
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
    :: (V.Unbox a, Monad m)
    => Int
    -> (b -> m (Maybe (a, b)))
    -> a
    -> b
    -> m (NonEmptyUVector a)
unfoldr1NM n f a b = fmap (cons a . unsafeFromVector) (V.unfoldrNM n f b)
{-# INLINE unfoldr1NM #-}

-- | /O(n)/ Construct a non-empty vector with n elements by repeatedly applying the
-- generator function to the already constructed part of the vector.
--
-- If 'constructN' does not create meaningful values, 'Nothing' is
-- returned. Otherwise, 'Just' containing a non-empty vector is returned.
--
constructN :: V.Unbox a => Int -> (Vector a -> a) -> Maybe (NonEmptyUVector a)
constructN n f = fromVector (V.constructN n f)
{-# INLINE constructN #-}

-- | /O(n)/ Construct a vector with n elements from right to left by repeatedly
-- applying the generator function to the already constructed part of the vector.
--
-- If 'constructrN' does not create meaningful values, 'Nothing' is
-- returned. Otherwise, 'Just' containing a non-empty vector is returned.
--
constructrN :: V.Unbox a => Int -> (Vector a -> a) -> Maybe (NonEmptyUVector a)
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
enumFromN :: (V.Unbox a, Num a) => a -> Int -> Maybe (NonEmptyUVector a)
enumFromN a n = fromVector (V.enumFromN a n)
{-# INLINE enumFromN #-}

-- | /O(n)/ Yield a non-emptyvector of length @max n 1@ containing the
-- values x, x+1 etc. This operation is usually more efficient than
-- 'enumFromTo'.
--
enumFromN1 :: (V.Unbox a, Num a) => a -> Int -> NonEmptyUVector a
enumFromN1 a n = unsafeFromVector (V.enumFromN a (max n 1))
{-# INLINE enumFromN1 #-}

-- | /O(n)/ Yield a non-empty vector of the given length containing the
-- values x, x+y, x+y+y etc. This operations is usually more efficient than
-- 'enumFromThenTo'.
--
-- If an enumeration does not use meaningful indices, 'Nothing' is returned,
-- otherwise, 'Just' containing a non-empty vector.
--
enumFromStepN :: (V.Unbox a, Num a) => a -> a -> Int -> Maybe (NonEmptyUVector a)
enumFromStepN a0 a1 n = fromVector (V.enumFromStepN a0 a1 n)
{-# INLINE enumFromStepN #-}

-- | /O(n)/ Yield a non-empty vector of length @max n 1@ containing the
-- values x, x+y, x+y+y etc. This operations is usually more efficient than
-- 'enumFromThenTo'.
--
enumFromStepN1 :: (V.Unbox a, Num a) => a -> a -> Int -> NonEmptyUVector a
enumFromStepN1 a0 a1 n = unsafeFromVector (V.enumFromStepN a0 a1 (max n 1))
{-# INLINE enumFromStepN1 #-}

-- | /O(n)/ Enumerate values from x to y.
--
-- If an enumeration does not use meaningful indices, 'Nothing' is returned,
-- otherwise, 'Just' containing a non-empty vector.
--
-- /WARNING/: This operation can be very inefficient. If at all possible,
-- use 'enumFromN' instead.
--
--
enumFromTo :: (V.Unbox a, Enum a) => a -> a -> Maybe (NonEmptyUVector a)
enumFromTo a0 a1 = fromVector (V.enumFromTo a0 a1)
{-# INLINE enumFromTo #-}

-- | /O(n)/ Enumerate values from x to y with a specific step z.
--
-- If an enumeration does not use meaningful indices, 'Nothing' is returned,
-- otherwise, 'Just' containing a non-empty vector.
--
-- /WARNING/: This operation can be very inefficient. If at all possible,
-- use 'enumFromStepN' instead.
enumFromThenTo :: (V.Unbox a, Enum a) => a -> a -> a -> Maybe (NonEmptyUVector a)
enumFromThenTo a0 a1 a2 = fromVector (V.enumFromThenTo a0 a1 a2)
{-# INLINE enumFromThenTo #-}

-- ---------------------------------------------------------------------- --
-- Concatenation

-- | /O(n)/ Prepend an element
--
-- >>> cons 1 (unsafeFromList [2,3])
-- [1,2,3]
--
cons :: V.Unbox a => a -> NonEmptyUVector a -> NonEmptyUVector a
cons a (NonEmptyUVector as) = consV a as
{-# INLINE cons #-}

-- | /O(n)/ Prepend an element to a Vector
--
-- >>> consV 1 (V.fromList [2,3])
-- [1,2,3]
--
consV :: V.Unbox a => a -> Vector a -> NonEmptyUVector a
consV a = NonEmptyUVector . V.cons a
{-# INLINE consV #-}

-- | /O(n)/ Append an element
--
-- >>> snoc (unsafeFromList [1,2]) 3
-- [1,2,3]
--
snoc :: V.Unbox a => NonEmptyUVector a -> a -> NonEmptyUVector a
snoc (NonEmptyUVector as) = snocV as
{-# INLINE snoc #-}

-- | /O(n)/ Append an element to a Vector
--
-- >>> snocV (V.fromList [1,2]) 3
-- [1,2,3]
--
snocV :: V.Unbox a => Vector a -> a -> NonEmptyUVector a
snocV as = NonEmptyUVector . V.snoc as
{-# INLINE snocV #-}

-- | /O(m+n)/ Concatenate two non-empty vectors
--
-- >>> (unsafeFromList [1..3]) ++ (unsafeFromList [4..6])
-- [1,2,3,4,5,6]
--
(++) :: V.Unbox a => NonEmptyUVector a -> NonEmptyUVector a -> NonEmptyUVector a
NonEmptyUVector v ++ NonEmptyUVector v' = NonEmptyUVector (v <> v')
{-# INLINE (++) #-}

-- | /O(n)/ Concatenate all non-empty vectors in the list
--
-- If list is empty, 'Nothing' is returned, otherwise 'Just'
-- containing the concatenated non-empty vectors
--
-- >>> concat [(unsafeFromList [1..3]), (unsafeFromList [4..6])]
-- Just [1,2,3,4,5,6]
--
concat :: V.Unbox a => [NonEmptyUVector a] -> Maybe (NonEmptyUVector a)
concat [] = Nothing
concat (a:as) = Just (concat1 (a :| as))
{-# INLINE concat #-}

-- | O(n) Concatenate all non-empty vectors in a non-empty list.
--
-- >>> concat1 ((unsafeFromList [1..3]) :| [(unsafeFromList [4..6])])
-- [1,2,3,4,5,6]
--
concat1 :: V.Unbox a => NonEmpty (NonEmptyUVector a) -> NonEmptyUVector a
concat1 = NonEmptyUVector . Foldable.foldl' go V.empty
  where
    go v (NonEmptyUVector a) = v <> a
{-# INLINE concat1 #-}

-- ---------------------------------------------------------------------- --
-- Conversions

-- | /O(n)/ Convert a non-empty vector to a non-empty list.
--
-- >>> toNonEmpty (unsafeFromList [1..3])
-- 1 :| [2,3]
--
toNonEmpty :: V.Unbox a => NonEmptyUVector a -> NonEmpty a
toNonEmpty = NonEmpty.fromList . V.toList . _neUvec
{-# INLINE toNonEmpty #-}

-- | O(n) Convert from a non-empty list to a non-empty vector.
--
-- >>> fromNonEmpty (1 :| [2,3])
-- [1,2,3]
--
fromNonEmpty :: V.Unbox a => NonEmpty a -> NonEmptyUVector a
fromNonEmpty = NonEmptyUVector . V.fromList . Foldable.toList
{-# INLINE fromNonEmpty #-}

-- | O(n) Convert from the first n-elements of a non-empty list to a
-- non-empty vector.
--
-- Returns 'Nothing' if indices are <= 0, otherwise 'Just' containing
-- the non-empty vector.
--
-- >>> fromNonEmptyN 3 (1 :| [2..5])
-- Just [1,2,3]
--
-- >>> fromNonEmptyN 0 (1 :| [2..5])
-- Nothing
--
fromNonEmptyN :: V.Unbox a => Int -> NonEmpty a -> Maybe (NonEmptyUVector a)
fromNonEmptyN n a = fromVector (V.fromListN n (Foldable.toList a))
{-# INLINE fromNonEmptyN #-}

-- | O(n) Convert from the first n-elements of a non-empty list to a
-- non-empty vector. This is a safe version of `fromNonEmptyN` which
-- takes @max n 1@ of the first n-elements of the non-empty list.
--
--
-- >>> fromNonEmptyN1 3 (1 :| [2..5])
-- [1,2,3]
--
-- >>> fromNonEmptyN1 0 (1 :| [2..5])
-- [1]
--
fromNonEmptyN1 :: V.Unbox a => Int -> NonEmpty a -> NonEmptyUVector a
fromNonEmptyN1 n = unsafeFromVector
    . V.fromListN (max n 1)
    . Foldable.toList
{-# INLINE fromNonEmptyN1 #-}

-- | /O(1)/ Convert from a non-empty vector to a vector.
--
--
-- >>> let nev :: NonEmptyUVector Int = unsafeFromList [1..3] in toVector nev
-- [1,2,3]
--
toVector :: V.Unbox a => NonEmptyUVector a -> Vector a
toVector = _neUvec
{-# INLINE toVector #-}

-- | /O(1)/ Convert from a vector to a non-empty vector.
--
-- If the vector is empty, then 'Nothing' is returned,
-- otherwise 'Just' containing the non-empty vector.
--
-- >>> fromVector $ V.fromList [1..3]
-- Just [1,2,3]
--
-- >>> fromVector $ V.fromList []
-- Nothing
--
fromVector :: V.Unbox a => Vector a -> Maybe (NonEmptyUVector a)
fromVector v = if V.null v then Nothing else Just (NonEmptyUVector v)
{-# INLINE fromVector #-}

-- | /O(1)/ Convert from a vector to a non-empty vector without
-- checking bounds.
--
-- /Warning/: the onus is on the user to ensure that their vector
-- is not empty, otherwise all bets are off!
--
--
-- >>> unsafeFromVector $ V.fromList [1..3]
-- [1,2,3]
--
unsafeFromVector :: V.Unbox a => Vector a -> NonEmptyUVector a
unsafeFromVector = NonEmptyUVector
{-# INLINE unsafeFromVector #-}

-- | /O(n)/ Convert from a non-empty vector to a list.
--
--
-- >>> let nev :: NonEmptyUVector Int = unsafeFromList [1..3] in toList nev
-- [1,2,3]
--
toList :: V.Unbox a => NonEmptyUVector a -> [a]
toList = V.toList . _neUvec
{-# INLINE toList #-}

-- | /O(n)/ Convert from a list to a non-empty vector.
--
--
-- >>> fromList [1..3]
-- Just [1,2,3]
--
-- >>> fromList []
-- Nothing
--
fromList :: V.Unbox a => [a] -> Maybe (NonEmptyUVector a)
fromList = fromVector . V.fromList
{-# INLINE fromList #-}

-- | /O(n)/ Convert from a list to a non-empty vector.
--
-- /Warning/: the onus is on the user to ensure that their vector
-- is not empty, otherwise all bets are off!
--
-- >>> unsafeFromList [1..3]
-- [1,2,3]
--
unsafeFromList :: V.Unbox a => [a] -> NonEmptyUVector a
unsafeFromList = unsafeFromVector . V.fromList
{-# INLINE unsafeFromList #-}

-- | /O(n)/ Convert the first n elements of a list to a non-empty vector.
--
-- If the list is empty or <= 0 elements are chosen, 'Nothing' is
-- returned, otherwise 'Just' containing the non-empty vector
--
-- >>> fromListN 3 [1..5]
-- Just [1,2,3]
--
-- >>> fromListN 3 []
-- Nothing
--
-- >>> fromListN 0 [1..5]
-- Nothing
--
fromListN :: V.Unbox a => Int -> [a] -> Maybe (NonEmptyUVector a)
fromListN n as = fromVector (V.fromListN n as)
{-# INLINE fromListN #-}

-- ---------------------------------------------------------------------- --
-- Restricting memory usage

-- | /O(n)/ Yield the argument but force it not to retain any extra memory,
-- possibly by copying it.
--
force :: V.Unbox a => NonEmptyUVector a -> NonEmptyUVector a
force (NonEmptyUVector a) = NonEmptyUVector (V.force a)
{-# INLINE force #-}

-- ---------------------------------------------------------------------- --
-- Bulk Updates

-- | /O(m+n)/ For each pair (i,a) from the list, replace the non-empty vector
-- element at position i by a.
--
-- >>> unsafeFromList [1..3] // [(2,4)]
-- [1,2,4]
--
-- >>> unsafeFromList [1..3] // []
-- [1,2,3]
--
(//) :: V.Unbox a => NonEmptyUVector a -> [(Int, a)] -> NonEmptyUVector a
NonEmptyUVector v // us = NonEmptyUVector (v V.// us)
{-# INLINE (//) #-}

-- | O(m+n) For each pair (i,a) from the vector of index/value pairs,
-- replace the vector element at position i by a.
--
-- >>> unsafeFromList [1..3] `update` V.fromList [(2,4)]
-- [1,2,4]
--
-- >>> unsafeFromList [1..3] `update` V.empty
-- [1,2,3]
--
update :: V.Unbox a => NonEmptyUVector a -> Vector (Int, a) -> NonEmptyUVector a
update (NonEmptyUVector v) v' = NonEmptyUVector (V.update v v')
{-# INLINE update #-}

-- | /O(m+min(n1,n2))/ For each index i from the index vector and the
-- corresponding value a from the value vector, replace the element of
-- the initial vector at position i by a.
--
--
-- >>> update_ (unsafeFromList [1..3]) (V.fromList [2]) (V.fromList [4])
-- [1,2,4]
--
-- >>> update_ (unsafeFromList [1..3]) V.empty V.empty
-- [1,2,3]
--
update_ :: V.Unbox a => NonEmptyUVector a -> Vector Int -> Vector a -> NonEmptyUVector a
update_ (NonEmptyUVector v) is as = NonEmptyUVector (V.update_ v is as)
{-# INLINE update_ #-}

-- | Same as '(//)' but without bounds checking.
--
unsafeUpd :: V.Unbox a => NonEmptyUVector a -> [(Int, a)] -> NonEmptyUVector a
unsafeUpd (NonEmptyUVector v) us = NonEmptyUVector (V.unsafeUpd v us)
{-# INLINE unsafeUpd #-}

-- | Same as 'update' but without bounds checking.
--
unsafeUpdate :: V.Unbox a => NonEmptyUVector a -> Vector (Int, a) -> NonEmptyUVector a
unsafeUpdate (NonEmptyUVector v) us = NonEmptyUVector (V.unsafeUpdate v us)
{-# INLINE unsafeUpdate #-}

-- | Same as 'update_' but without bounds checking.
--
unsafeUpdate_ :: V.Unbox a => NonEmptyUVector a -> Vector Int -> Vector a -> NonEmptyUVector a
unsafeUpdate_ (NonEmptyUVector v) is as = NonEmptyUVector (V.unsafeUpdate_ v is as)
{-# INLINE unsafeUpdate_ #-}

-- ---------------------------------------------------------------------- --
-- Accumulation

-- | /O(m+n)/ For each pair @(i,b)@ from the non-empty list, replace the
-- non-empty vector element @a@ at position @i@ by @f a b@.
--
--
-- >>> accum (+) (unsafeFromList [1..3]) [(2,10)]
-- [1,2,13]
--
-- >>> accum (+) (unsafeFromList [1..3]) []
-- [1,2,3]
--
accum
    :: (V.Unbox a, V.Unbox b)
    => (a -> b -> a)
      -- ^ accumulating function @f@
    -> NonEmptyUVector a
      -- ^ initial non-empty vector (of length @m@)
    -> [(Int, b)]
      -- ^ list of index/value pairs (of length @n@)
    -> NonEmptyUVector a
accum f (NonEmptyUVector v) u = NonEmptyUVector (V.accum f v u)
{-# INLINE accum #-}

-- | /O(m+n)/ For each pair @(i,b)@ from the vector of pairs, replace the
-- non-empty vector element @a@ at position @i@ by @f a b@.
--
-- >>> accumulate (+) (unsafeFromList [1..3]) (V.fromList [(2,10)])
-- [1,2,13]
--
-- >>> accumulate (+) (unsafeFromList [1..3]) V.empty
-- [1,2,3]
--
accumulate
    :: (V.Unbox a, V.Unbox b)
    => (a -> b -> a)
      -- ^ accumulating function @f@
    -> NonEmptyUVector a
      -- ^ initial non-empty vector (of length @m@)
    -> Vector (Int, b)
      -- ^ vector of index/value pairs (of length @n@)
    -> NonEmptyUVector a
accumulate f (NonEmptyUVector v) u = NonEmptyUVector (V.accumulate f v u)
{-# INLINE accumulate #-}

-- | /O(m+min(n1,n2))/ For each index @i@ from the index vector and the
-- corresponding value @b@ from the the value vector, replace the element
-- of the initial non-empty vector at position @i@ by @f a b@.
--
-- >>> accumulate_ (+) (unsafeFromList [1..3]) (V.fromList [2]) (V.fromList [10])
-- [1,2,13]
--
-- >>> accumulate_ (+) (unsafeFromList [1..3]) V.empty V.empty
-- [1,2,3]
--
accumulate_
    :: (V.Unbox a, V.Unbox b)
    => (a -> b -> a)
      -- ^ accumulating function @f@
    -> NonEmptyUVector a
      -- ^ initial non-empty vector (of length @m@)
    -> Vector Int
       -- ^ vector of indices (of length @n1@)
    -> Vector b
       -- ^ vector of values (of length @n2@)
    -> NonEmptyUVector a
accumulate_ f (NonEmptyUVector v) i b = NonEmptyUVector (V.accumulate_ f v i b)
{-# INLINE accumulate_ #-}

-- | Same as 'accum' but without bounds checking.
--
unsafeAccum
    :: (V.Unbox a, V.Unbox b)
    => (a -> b -> a)
      -- ^ accumulating function @f@
    -> NonEmptyUVector a
      -- ^ initial non-empty vector (of length @m@)
    -> [(Int, b)]
      -- ^ list of index/value pairs (of length @n@)
    -> NonEmptyUVector a
unsafeAccum f (NonEmptyUVector v) u = NonEmptyUVector (V.unsafeAccum f v u)
{-# INLINE unsafeAccum #-}

-- | Same as 'accumulate' but without bounds checking.
--
unsafeAccumulate
    :: (V.Unbox a, V.Unbox b) 
    => (a -> b -> a)
      -- ^ accumulating function @f@
    -> NonEmptyUVector a
      -- ^ initial non-empty vector (of length @m@)
    -> Vector (Int, b)
      -- ^ vector of index/value pairs (of length @n@)
    -> NonEmptyUVector a
unsafeAccumulate f v u = NonEmptyUVector (V.unsafeAccumulate f v' u)
  where
    v' = _neUvec v
{-# INLINE unsafeAccumulate #-}

-- | Same as 'accumulate_' but without bounds checking.
--
unsafeAccumulate_
    :: (V.Unbox a, V.Unbox b)
    => (a -> b -> a)
      -- ^ accumulating function @f@
    -> NonEmptyUVector a
      -- ^ initial non-empty vector (of length @m@)
    -> Vector Int
      -- ^ vector of indices of length @n1@
    -> Vector b
      -- ^ vector of values (of length @n2@)
    -> NonEmptyUVector a
unsafeAccumulate_ f v i b = NonEmptyUVector (V.unsafeAccumulate_ f v' i b)
  where
    v' = _neUvec v
{-# INLINE unsafeAccumulate_ #-}

-- ---------------------------------------------------------------------- --
-- Permutations

-- | /O(n)/ Reverse a non-empty vector
--
-- >>> reverse $ unsafeFromList [1..3]
-- [3,2,1]
--
reverse :: V.Unbox a => NonEmptyUVector a -> NonEmptyUVector a
reverse = NonEmptyUVector . V.reverse . _neUvec
{-# INLINE reverse #-}

-- | /O(n)/ Yield the non-empty vector obtained by replacing each element
-- @i@ of the non-empty index vector by @xs'!'i@. This is equivalent to
-- @'map' (xs'!') is@ but is often much more efficient.
--
-- >>> backpermute (unsafeFromList [1..3]) (unsafeFromList [2,0])
-- [3,1]
--
backpermute :: V.Unbox a => NonEmptyUVector a -> NonEmptyUVector Int -> NonEmptyUVector a
backpermute (NonEmptyUVector v) (NonEmptyUVector i)
    = NonEmptyUVector (V.backpermute v i)
{-# INLINE backpermute #-}

-- | Same as 'backpermute' but without bounds checking.
--
unsafeBackpermute
    :: V.Unbox a
    => NonEmptyUVector a
    -> NonEmptyUVector Int
    -> NonEmptyUVector a
unsafeBackpermute (NonEmptyUVector v) (NonEmptyUVector i)
    = NonEmptyUVector (V.unsafeBackpermute v i)
{-# INLINE unsafeBackpermute #-}

-- ---------------------------------------------------------------------- --
-- Safe destructive updates

-- | Apply a destructive operation to a non-empty vector. The operation
-- will be performed in place if it is safe to do so and will modify a
-- copy of the non-empty vector otherwise.
--
modify
    :: V.Unbox a
    => (forall s. MVector s a -> ST s ())
    -> NonEmptyUVector a
    -> NonEmptyUVector a
modify p (NonEmptyUVector v) = NonEmptyUVector (V.modify p v)
{-# INLINE modify #-}

-- ---------------------------------------------------------------------- --
-- Indexing

-- | /O(n)/ Pair each element in a vector with its index.
--
-- >>> indexed $ unsafeFromList ["a","b","c"]
-- [(0,"a"),(1,"b"),(2,"c")]
--
indexed :: (V.Unbox a) => NonEmptyUVector a -> NonEmptyUVector (Int, a)
indexed = NonEmptyUVector . V.indexed . _neUvec
{-# INLINE indexed #-}

-- ---------------------------------------------------------------------- --
-- Mapping

-- | /O(n)/ Map a function over a non-empty vector.
--
-- >>> map (+1) $ unsafeFromList [1..3]
-- [2,3,4]
--
map :: (V.Unbox a, V.Unbox b) => (a -> b) -> NonEmptyUVector a -> NonEmptyUVector b
map f = NonEmptyUVector . V.map f . _neUvec
{-# INLINE map #-}

-- | /O(n)/ Apply a function to every element of a non-empty vector and
-- its index.
--
-- >>> imap (\i a -> if i == 2 then a+1 else a+0) $ unsafeFromList [1..3]
-- [1,2,4]
--
imap :: (V.Unbox a, V.Unbox b) => (Int -> a -> b) -> NonEmptyUVector a -> NonEmptyUVector b
imap f = NonEmptyUVector . V.imap f . _neUvec
{-# INLINE imap #-}

-- | Map a function over a vector and concatenate the results.
--
-- >>> concatMap (\a -> unsafeFromList [a,a]) (unsafeFromList [1,2,3])
-- [1,1,2,2,3,3]
--
concatMap
    :: (V.Unbox a, V.Unbox b)
    => (a -> NonEmptyUVector b)
    -> NonEmptyUVector a
    -> NonEmptyUVector b
concatMap f = NonEmptyUVector . V.concatMap (_neUvec . f) . _neUvec
{-# INLINE concatMap #-}

-- ---------------------------------------------------------------------- --
-- Monadic Mapping

-- | /O(n)/ Apply the monadic action to all elements of the non-empty
-- vector, yielding non-empty vector of results.
--
-- >>> mapM Just (unsafeFromList [1..3])
-- Just [1,2,3]
--
-- >>> mapM (const Nothing) (unsafeFromList [1..3])
-- Nothing
--
mapM :: (V.Unbox a, V.Unbox b, Monad m) => (a -> m b) -> NonEmptyUVector a -> m (NonEmptyUVector b)
mapM f = fmap NonEmptyUVector . V.mapM f . _neUvec
{-# INLINE mapM #-}

-- | /O(n)/ Apply the monadic action to every element of a non-empty
-- vector and its index, yielding a non-empty vector of results.
--
-- >>> imapM (\i a -> if i == 1 then Just a else Just 0) (unsafeFromList [1..3])
-- Just [0,2,0]
--
-- >>> imapM (\_ _ -> Nothing) (unsafeFromList [1..3])
-- Nothing
--
imapM
    :: (V.Unbox a, V.Unbox b, Monad m)
    => (Int -> a -> m b)
    -> NonEmptyUVector a
    -> m (NonEmptyUVector b)
imapM f = fmap NonEmptyUVector . V.imapM f . _neUvec
{-# INLINE imapM #-}

-- | /O(n)/ Apply the monadic action to all elements of a non-empty vector
-- and ignore the results.
--
-- >>> mapM_ (const $ Just ()) (unsafeFromList [1..3])
-- Just ()
--
-- >>> mapM_ (const Nothing) (unsafeFromList [1..3])
-- Nothing
--
mapM_ :: (V.Unbox a, Monad m) => (a -> m b) -> NonEmptyUVector a -> m ()
mapM_ f = V.mapM_ f . _neUvec
{-# INLINE mapM_ #-}

-- | /O(n)/ Apply the monadic action to every element of a non-emptpy
-- vector and its index, ignoring the results
--
-- >>> imapM_ (\i a -> if i == 1 then P.print a else P.putStrLn "0") (unsafeFromList [1..3])
-- 0
-- 2
-- 0
--
-- >>> imapM_ (\_ _ -> Nothing) (unsafeFromList [1..3])
-- Nothing
--
imapM_ :: (V.Unbox a, Monad m) => (Int -> a -> m b) -> NonEmptyUVector a -> m ()
imapM_ f = V.imapM_ f . _neUvec
{-# INLINE imapM_ #-}

-- | /O(n)/ Apply the monadic action to all elements of the non-empty
-- vector, yielding a  non0empty vector of results.
--
-- Equivalent to @flip 'mapM'@.
--
forM :: (V.Unbox a, V.Unbox b, Monad m) => NonEmptyUVector a -> (a -> m b) -> m (NonEmptyUVector b)
forM (NonEmptyUVector v) f = fmap NonEmptyUVector (V.forM v f)
{-# INLINE forM #-}

-- | /O(n)/ Apply the monadic action to all elements of a non-empty
-- vector and ignore the results.
--
-- Equivalent to @flip 'mapM_'@.
--
forM_ :: (V.Unbox a, Monad m) => NonEmptyUVector a -> (a -> m b) -> m ()
forM_ (NonEmptyUVector v) f = V.forM_ v f
{-# INLINE forM_ #-}

-- ---------------------------------------------------------------------- --
-- Zipping

-- | /O(min(m,n))/ Zip two non-empty vectors with the given function.
--
-- >>> zipWith (+) (unsafeFromList [1..3]) (unsafeFromList [1..3])
-- [2,4,6]
--
zipWith
    :: (V.Unbox a, V.Unbox b, V.Unbox c)
    => (a -> b -> c)
    -> NonEmptyUVector a
    -> NonEmptyUVector b
    -> NonEmptyUVector c
zipWith f a b = NonEmptyUVector (V.zipWith f a' b')
  where
    a' = _neUvec a
    b' = _neUvec b
{-# INLINE zipWith #-}

-- | Zip three non-empty vectors with the given function.
--
--
zipWith3
    :: (V.Unbox a, V.Unbox b, V.Unbox c, V.Unbox d)
    => (a -> b -> c -> d)
    -> NonEmptyUVector a
    -> NonEmptyUVector b
    -> NonEmptyUVector c
    -> NonEmptyUVector d
zipWith3 f a b c = NonEmptyUVector (V.zipWith3 f a' b' c')
  where
    a' = _neUvec a
    b' = _neUvec b
    c' = _neUvec c
{-# INLINE zipWith3 #-}

-- | Zip four non-empty vectors with the given function.
--
zipWith4
    :: (V.Unbox a, V.Unbox b, V.Unbox c, V.Unbox d, V.Unbox e)
    => (a -> b -> c -> d -> e)
    -> NonEmptyUVector a
    -> NonEmptyUVector b
    -> NonEmptyUVector c
    -> NonEmptyUVector d
    -> NonEmptyUVector e
zipWith4 f a b c d = NonEmptyUVector (V.zipWith4 f a' b' c' d')
  where
    a' = _neUvec a
    b' = _neUvec b
    c' = _neUvec c
    d' = _neUvec d
{-# INLINE zipWith4 #-}

-- | Zip five non-empty vectors with the given function.
--
zipWith5
    :: (V.Unbox a, V.Unbox b, V.Unbox c, V.Unbox d, V.Unbox e, V.Unbox f)
    => (a -> b -> c -> d -> e -> f)
    -> NonEmptyUVector a
    -> NonEmptyUVector b
    -> NonEmptyUVector c
    -> NonEmptyUVector d
    -> NonEmptyUVector e
    -> NonEmptyUVector f
zipWith5 f a b c d e = NonEmptyUVector (V.zipWith5 f a' b' c' d' e')
  where
    a' = _neUvec a
    b' = _neUvec b
    c' = _neUvec c
    d' = _neUvec d
    e' = _neUvec e
{-# INLINE zipWith5 #-}

-- | Zip six non-empty vectors with the given function.
--
zipWith6
    :: (V.Unbox a, V.Unbox b, V.Unbox c, V.Unbox d, V.Unbox e, V.Unbox f, V.Unbox g)
    => (a -> b -> c -> d -> e -> f -> g)
    -> NonEmptyUVector a
    -> NonEmptyUVector b
    -> NonEmptyUVector c
    -> NonEmptyUVector d
    -> NonEmptyUVector e
    -> NonEmptyUVector f
    -> NonEmptyUVector g
zipWith6 k a b c d e f = NonEmptyUVector (V.zipWith6 k a' b' c' d' e' f')
  where
    a' = _neUvec a
    b' = _neUvec b
    c' = _neUvec c
    d' = _neUvec d
    e' = _neUvec e
    f' = _neUvec f
{-# INLINE zipWith6 #-}


-- | /O(min(m,n))/ Zip two non-empty vectors with a function that also
-- takes the elements' indices.
--
izipWith
    :: (V.Unbox a, V.Unbox b, V.Unbox c)
    => (Int -> a -> b -> c)
    -> NonEmptyUVector a
    -> NonEmptyUVector b
    -> NonEmptyUVector c
izipWith f a b = NonEmptyUVector (V.izipWith f a' b')
  where
    a' = _neUvec a
    b' = _neUvec b
{-# INLINE izipWith #-}

-- | Zip three non-empty vectors and their indices with the given function.
--
izipWith3
    :: (V.Unbox a, V.Unbox b, V.Unbox c, V.Unbox d)
    => (Int -> a -> b -> c -> d)
    -> NonEmptyUVector a
    -> NonEmptyUVector b
    -> NonEmptyUVector c
    -> NonEmptyUVector d
izipWith3 f a b c = NonEmptyUVector (V.izipWith3 f a' b' c')
  where
    a' = _neUvec a
    b' = _neUvec b
    c' = _neUvec c
{-# INLINE izipWith3 #-}

-- | Zip four non-empty vectors and their indices with the given function.
--
izipWith4
    :: (V.Unbox a, V.Unbox b, V.Unbox c, V.Unbox d, V.Unbox e)
    => (Int -> a -> b -> c -> d -> e)
    -> NonEmptyUVector a
    -> NonEmptyUVector b
    -> NonEmptyUVector c
    -> NonEmptyUVector d
    -> NonEmptyUVector e
izipWith4 f a b c d = NonEmptyUVector (V.izipWith4 f a' b' c' d')
  where
    a' = _neUvec a
    b' = _neUvec b
    c' = _neUvec c
    d' = _neUvec d
{-# INLINE izipWith4 #-}

-- | Zip five non-empty vectors and their indices with the given function.
--
izipWith5
    :: (V.Unbox a, V.Unbox b, V.Unbox c, V.Unbox d, V.Unbox e, V.Unbox f)
    => (Int -> a -> b -> c -> d -> e -> f)
    -> NonEmptyUVector a
    -> NonEmptyUVector b
    -> NonEmptyUVector c
    -> NonEmptyUVector d
    -> NonEmptyUVector e
    -> NonEmptyUVector f
izipWith5 f a b c d e = NonEmptyUVector (V.izipWith5 f a' b' c' d' e')
  where
    a' = _neUvec a
    b' = _neUvec b
    c' = _neUvec c
    d' = _neUvec d
    e' = _neUvec e
{-# INLINE izipWith5 #-}

-- | Zip six non-empty vectors and their indices with the given function.
--
izipWith6
    :: (V.Unbox a, V.Unbox b, V.Unbox c, V.Unbox d, V.Unbox e, V.Unbox f, V.Unbox g)
    => (Int -> a -> b -> c -> d -> e -> f -> g)
    -> NonEmptyUVector a
    -> NonEmptyUVector b
    -> NonEmptyUVector c
    -> NonEmptyUVector d
    -> NonEmptyUVector e
    -> NonEmptyUVector f
    -> NonEmptyUVector g
izipWith6 k a b c d e f = NonEmptyUVector (V.izipWith6 k a' b' c' d' e' f')
  where
    a' = _neUvec a
    b' = _neUvec b
    c' = _neUvec c
    d' = _neUvec d
    e' = _neUvec e
    f' = _neUvec f
{-# INLINE izipWith6 #-}

-- | /O(min(n,m))/ Elementwise pairing of non-empty vector elements. This is a special case
-- of 'zipWith' where the function argument is @(,)@
--
zip :: (V.Unbox a, V.Unbox b) => NonEmptyUVector a -> NonEmptyUVector b -> NonEmptyUVector (a, b)
zip a b = NonEmptyUVector (V.zip a' b')
  where
    a' = _neUvec a
    b' = _neUvec b
{-# INLINE zip #-}

-- | Zip together three non-empty vectors.
--
zip3
    :: (V.Unbox a, V.Unbox b, V.Unbox c)
    => NonEmptyUVector a
    -> NonEmptyUVector b
    -> NonEmptyUVector c
    -> NonEmptyUVector (a, b, c)
zip3 a b c = NonEmptyUVector (V.zip3 a' b' c')
  where
    a' = _neUvec a
    b' = _neUvec b
    c' = _neUvec c
{-# INLINE zip3 #-}

-- | Zip together four non-empty vectors.
--
zip4
    :: (V.Unbox a, V.Unbox b, V.Unbox c, V.Unbox d)
    => NonEmptyUVector a
    -> NonEmptyUVector b
    -> NonEmptyUVector c
    -> NonEmptyUVector d
    -> NonEmptyUVector (a, b, c, d)
zip4 a b c d = NonEmptyUVector (V.zip4 a' b' c' d')
  where
    a' = _neUvec a
    b' = _neUvec b
    c' = _neUvec c
    d' = _neUvec d
{-# INLINE zip4 #-}

-- | Zip together five non-empty vectors.
--
zip5
    :: (V.Unbox a, V.Unbox b, V.Unbox c, V.Unbox d, V.Unbox e)
    => NonEmptyUVector a
    -> NonEmptyUVector b
    -> NonEmptyUVector c
    -> NonEmptyUVector d
    -> NonEmptyUVector e
    -> NonEmptyUVector (a, b, c, d, e)
zip5 a b c d e = NonEmptyUVector (V.zip5 a' b' c' d' e')
  where
    a' = _neUvec a
    b' = _neUvec b
    c' = _neUvec c
    d' = _neUvec d
    e' = _neUvec e
{-# INLINE zip5 #-}

-- | Zip together six non-empty vectors.
--
zip6
    :: (V.Unbox a, V.Unbox b, V.Unbox c, V.Unbox d, V.Unbox e, V.Unbox f)
    => NonEmptyUVector a
    -> NonEmptyUVector b
    -> NonEmptyUVector c
    -> NonEmptyUVector d
    -> NonEmptyUVector e
    -> NonEmptyUVector f
    -> NonEmptyUVector (a, b, c, d, e, f)
zip6 a b c d e f = NonEmptyUVector (V.zip6 a' b' c' d' e' f')
  where
    a' = _neUvec a
    b' = _neUvec b
    c' = _neUvec c
    d' = _neUvec d
    e' = _neUvec e
    f' = _neUvec f
{-# INLINE zip6 #-}

-- ---------------------------------------------------------------------- --
-- Monadic Zipping

-- | /O(min(m,n))/ Zip the two non-empty vectors with the monadic action
-- and yield a non-empty vector of results.
--
zipWithM
    :: (V.Unbox a, V.Unbox b, V.Unbox c, Monad m)
    => (a -> b -> m c)
    -> NonEmptyUVector a
    -> NonEmptyUVector b
    -> m (NonEmptyUVector c)
zipWithM f a b = fmap NonEmptyUVector (V.zipWithM f a' b')
  where
    a' = _neUvec a
    b' = _neUvec b
{-# INLINE zipWithM #-}

-- | /O(min(m,n))/ Zip the two non-empty vectors with a monadic action
-- that also takes the element index and yield a vector of results.
--
izipWithM
    :: (V.Unbox a, V.Unbox b, V.Unbox c, Monad m)
    => (Int -> a -> b -> m c)
    -> NonEmptyUVector a
    -> NonEmptyUVector b
    -> m (NonEmptyUVector c)
izipWithM f a b = fmap NonEmptyUVector (V.izipWithM f a' b')
  where
    a' = _neUvec a
    b' = _neUvec b
{-# INLINE izipWithM #-}

-- | /O(min(m,n))/ Zip the two non-empty vectors with the monadic action
-- and ignore the results.
--
zipWithM_
    :: (V.Unbox a, V.Unbox b, Monad m)
    => (a -> b -> m c)
    -> NonEmptyUVector a
    -> NonEmptyUVector b
    -> m ()
zipWithM_ f a b = V.zipWithM_ f (_neUvec a) (_neUvec b)
{-# INLINE zipWithM_ #-}

-- | /O(min(m,n))/ Zip the two non-empty vectors with a monadic action
-- that also takes the element index and ignore the results.
--
izipWithM_
    :: (V.Unbox a, V.Unbox b, Monad m)
    => (Int -> a -> b -> m c)
    -> NonEmptyUVector a
    -> NonEmptyUVector b
    -> m ()
izipWithM_ f a b = V.izipWithM_ f (_neUvec a) (_neUvec b)
{-# INLINE izipWithM_ #-}

-- ---------------------------------------------------------------------- --
-- Unzipping

-- | /O(min(m,n))/ Unzip a non-empty vector of pairs.
--
unzip :: (V.Unbox a, V.Unbox b) => NonEmptyUVector (a, b) -> (NonEmptyUVector a, NonEmptyUVector b)
unzip (NonEmptyUVector v) = case V.unzip v of
    ~(a,b) -> (NonEmptyUVector a, NonEmptyUVector b)
{-# INLINE unzip #-}

-- | Unzip a non-empty vector of triples.
--
unzip3
    :: (V.Unbox a, V.Unbox b, V.Unbox c) 
    => NonEmptyUVector (a, b, c)
    -> (NonEmptyUVector a, NonEmptyUVector b, NonEmptyUVector c)
unzip3 (NonEmptyUVector v) = case V.unzip3 v of
    ~(a,b,c) ->
      ( NonEmptyUVector a
      , NonEmptyUVector b
      , NonEmptyUVector c
      )
{-# INLINE unzip3 #-}

-- | Unzip a non-empty vector of quadruples.
--
unzip4
    :: (V.Unbox a, V.Unbox b, V.Unbox c, V.Unbox d) 
    => NonEmptyUVector (a, b, c, d)
    -> ( NonEmptyUVector a
       , NonEmptyUVector b
       , NonEmptyUVector c
       , NonEmptyUVector d
       )
unzip4 (NonEmptyUVector v) = case V.unzip4 v of
    ~(a,b,c,d) ->
      ( NonEmptyUVector a
      , NonEmptyUVector b
      , NonEmptyUVector c
      , NonEmptyUVector d
      )
{-# INLINE unzip4 #-}

-- | Unzip a non-empty vector of quintuples.
--
unzip5
    :: (V.Unbox a, V.Unbox b, V.Unbox c, V.Unbox d, V.Unbox e) 
    => NonEmptyUVector (a, b, c, d, e)
    -> ( NonEmptyUVector a
       , NonEmptyUVector b
       , NonEmptyUVector c
       , NonEmptyUVector d
       , NonEmptyUVector e
       )
unzip5 (NonEmptyUVector v) = case V.unzip5 v of
    ~(a,b,c,d,e) ->
      ( NonEmptyUVector a
      , NonEmptyUVector b
      , NonEmptyUVector c
      , NonEmptyUVector d
      , NonEmptyUVector e
      )
{-# INLINE unzip5 #-}

-- | Unzip a non-empty vector of sextuples.
--
unzip6
    :: (V.Unbox a, V.Unbox b, V.Unbox c, V.Unbox d, V.Unbox e, V.Unbox f) 
    => NonEmptyUVector (a, b, c, d, e, f)
    -> ( NonEmptyUVector a
       , NonEmptyUVector b
       , NonEmptyUVector c
       , NonEmptyUVector d
       , NonEmptyUVector e
       , NonEmptyUVector f
       )
unzip6 (NonEmptyUVector v) = case V.unzip6 v of
    ~(a,b,c,d,e,f) ->
      ( NonEmptyUVector a
      , NonEmptyUVector b
      , NonEmptyUVector c
      , NonEmptyUVector d
      , NonEmptyUVector e
      , NonEmptyUVector f
      )
{-# INLINE unzip6 #-}

-- ---------------------------------------------------------------------- --
-- Filtering

-- | /O(n)/ Drop elements that do not satisfy the predicate.
--
-- If no elements satisfy the predicate, the resulting vector may be empty.
--
-- >>> filter (\a -> if a == 2 then False else True) (unsafeFromList [1..3])
-- [1,3]
--
-- >>> filter (const False) (unsafeFromList [1..3])
-- []
--
filter :: V.Unbox a => (a -> Bool) -> NonEmptyUVector a -> Vector a
filter f = V.filter f . _neUvec
{-# INLINE filter #-}

-- | /O(n)/ Drop elements that do not satisfy the predicate which is
-- applied to values and their indices.
--
-- If no elements satisfy the predicate, the resulting vector may be empty.
--
-- >>> ifilter (\i a -> if a == 2 || i == 0 then False else True) (unsafeFromList [1..3])
-- [3]
--
-- >>> ifilter (\_ _ -> False) (unsafeFromList [1..3])
-- []
--
ifilter
    :: V.Unbox a
    => (Int -> a -> Bool)
    -> NonEmptyUVector a
    -> Vector a
ifilter f = V.ifilter f . _neUvec
{-# INLINE ifilter #-}

-- | /O(n)/ Drop elements that do not satisfy the monadic predicate.
--
-- If no elements satisfy the predicate, the resulting vector may be empty.
--
-- >>> filterM (\a -> if a == 2 then Just False else Just True) (unsafeFromList [1..3])
-- Just [1,3]
--
-- >>> filterM (\a -> if a == 2 then Nothing else Just True) (unsafeFromList [1..3])
-- Nothing
--
-- >>> filterM (const $ Just False) (unsafeFromList [1..3])
-- Just []
--
filterM
    :: (V.Unbox a, Monad m)
    => (a -> m Bool)
    -> NonEmptyUVector a
    -> m (Vector a)
filterM f = V.filterM f . _neUvec
{-# INLINE filterM #-}

-- | /O(n)/ Drop elements that do not satisfy the monadic predicate that is
-- a function of index and value.
--
-- If no elements satisfy the predicate, the resulting vector may be empty.
--
-- TODO: this should be a more efficient function in @vector@.
--
-- >>> ifilterM (\i a -> if a == 2 || i == 0 then Just False else Just True) (unsafeFromList [1..3])
-- Just [3]
--
-- >>> ifilterM (\i a -> if a == 2 || i == 0 then Nothing else Just True) (unsafeFromList [1..3])
-- Nothing
--
-- >>> ifilterM (\_ _ -> Just False) (unsafeFromList [1..3])
-- Just []
--
ifilterM
    :: (V.Unbox a, Monad m)
    => (Int -> a -> m Bool)
    -> NonEmptyUVector a
    -> m (Vector a)
ifilterM f = fmap (V.map snd)
    . V.filterM (uncurry f)
    . V.indexed
    . _neUvec
{-# INLINE ifilterM #-}

-- | /O(n)/ Drop repeated adjacent elements.
--
-- >>> uniq $ unsafeFromList [1,1,2,2,3,3,1]
-- [1,2,3,1]
--
uniq :: (V.Unbox a, Eq a) => NonEmptyUVector a -> NonEmptyUVector a
uniq = NonEmptyUVector . V.uniq . _neUvec
{-# INLINE uniq #-}

-- | /O(n)/ Drop elements when predicate returns Nothing
--
-- If no elements satisfy the predicate, the resulting vector may be empty.
--
-- >>> mapMaybe (\a -> if a == 2 then Nothing else Just a) (unsafeFromList [1..3])
-- [1,3]
--
mapMaybe
    :: (V.Unbox a, V.Unbox b)
    => (a -> Maybe b)
    -> NonEmptyUVector a
    -> Vector b
mapMaybe f = V.mapMaybe f . _neUvec
{-# INLINE mapMaybe #-}

-- | /O(n)/ Drop elements when predicate, applied to index and value, returns Nothing
--
-- If no elements satisfy the predicate, the resulting vector may be empty.
--
-- >>> imapMaybe (\i a -> if a == 2 || i == 2 then Nothing else Just a) (unsafeFromList [1..3])
-- [1]
--
imapMaybe
    :: (V.Unbox a, V.Unbox b) 
    => (Int -> a -> Maybe b)
    -> NonEmptyUVector a
    -> Vector b
imapMaybe f = V.imapMaybe f . _neUvec
{-# INLINE imapMaybe #-}

-- | /O(n)/ Yield the longest prefix of elements satisfying the predicate
-- without copying.
--
-- If no elements satisfy the predicate, the resulting vector may be empty.
--
-- >>> takeWhile (/= 3) (unsafeFromList [1..3])
-- [1,2]
--
takeWhile :: V.Unbox a => (a -> Bool) -> NonEmptyUVector a -> Vector a
takeWhile f = V.takeWhile f . _neUvec
{-# INLINE takeWhile #-}

-- | /O(n)/ Drop the longest prefix of elements that satisfy the predicate
-- without copying.
--
-- If all elements satisfy the predicate, the resulting vector may be empty.
--
-- >>> dropWhile (/= 3) (unsafeFromList [1..3])
-- [3]
--
dropWhile :: V.Unbox a => (a -> Bool) -> NonEmptyUVector a -> Vector a
dropWhile f = V.dropWhile f . _neUvec
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
-- >>> partition (< 3) (unsafeFromList [1..5])
-- ([1,2],[3,4,5])
--
partition :: V.Unbox a => (a -> Bool) -> NonEmptyUVector a -> (Vector a, Vector a)
partition f = V.partition f . _neUvec
{-# INLINE partition #-}

-- | /O(n)/ Split the non-empty vector in two parts, the first one
-- containing the Left elements and the second containing the
-- Right elements. The relative order of the elements is preserved.
--
-- If all elements produce a Left (or Right), one of the
-- resulting vectors may be empty.
--
-- >>> partitionWith (\a -> if a < 3 then Left a else Right (P.show a)) (unsafeFromList [1..5])
-- ([1,2],["3","4","5"])
--
partitionWith :: (V.Unbox a, V.Unbox b, V.Unbox c) => 
  (a -> Either b c) -> NonEmptyUVector a -> (Vector b, Vector c)
partitionWith f = V.partitionWith f . _neUvec
{-# INLINE partitionWith #-}

-- | /O(n)/ Split the non-empty vector in two parts, the first one
-- containing those elements that satisfy the predicate and the second
-- one those that don't. The order of the elements is not preserved but
-- the operation is often faster than 'partition'.
--
-- If all or no elements satisfy the predicate, one of the resulting vectors
-- may be empty.
--
unstablePartition
    :: V.Unbox a
    => (a -> Bool)
    -> NonEmptyUVector a
    -> (Vector a, Vector a)
unstablePartition f = V.unstablePartition f . _neUvec
{-# INLINE unstablePartition #-}

-- | /O(n)/ Split the non-empty vector into the longest prefix of elements
-- that satisfy the predicate and the rest without copying.
--
-- If all or no elements satisfy the predicate, one of the resulting vectors
-- may be empty.
--
-- >>> span (== 1) (unsafeFromList [1,1,2,3,1])
-- ([1,1],[2,3,1])
--
span :: V.Unbox a => (a -> Bool) -> NonEmptyUVector a -> (Vector a, Vector a)
span f = V.span f . _neUvec
{-# INLINE span #-}

-- | /O(n)/ Split the vector into the longest prefix of elements that do not
-- satisfy the predicate and the rest without copying.
--
-- If all or no elements satisfy the predicate, one of the resulting vectors
-- may be empty.
--
-- >>> break (== 2) (unsafeFromList [1,1,2,3,1])
-- ([1,1],[2,3,1])
--
break :: V.Unbox a => (a -> Bool) -> NonEmptyUVector a -> (Vector a, Vector a)
break f = V.break f . _neUvec
{-# INLINE break #-}

-- ---------------------------------------------------------------------- --
-- Searching

-- | /O(n)/ Check if the non-empty vector contains an element
--
-- >>> elem 1 $ unsafeFromList [1..3]
-- True
-- >>> elem 4 $ unsafeFromList [1..3]
-- False
--
elem :: (V.Unbox a, Eq a) => a -> NonEmptyUVector a -> Bool
elem a = V.elem a . _neUvec
{-# INLINE elem #-}

-- | /O(n)/ Check if the non-empty vector does not contain an element
-- (inverse of 'elem')
--
-- >>> notElem 1 $ unsafeFromList [1..3]
-- False
--
-- >>> notElem 4 $ unsafeFromList [1..3]
-- True
--
notElem :: (V.Unbox a, Eq a) => a -> NonEmptyUVector a -> Bool
notElem a = V.notElem a . _neUvec
{-# INLINE notElem #-}

-- | /O(n)/ Yield 'Just' the first element matching the predicate or
-- 'Nothing' if no such element exists.
--
-- >>> find (< 2) $ unsafeFromList [1..3]
-- Just 1
--
-- >>> find (< 0) $ unsafeFromList [1..3]
-- Nothing
--
find :: V.Unbox a => (a -> Bool) -> NonEmptyUVector a -> Maybe a
find f = V.find f . _neUvec
{-# INLINE find #-}

-- | /O(n)/ Yield 'Just' the index of the first element matching the
-- predicate or 'Nothing' if no such element exists.
--
-- >>> findIndex (< 2) $ unsafeFromList [1..3]
-- Just 0
--
-- >>> findIndex (< 0) $ unsafeFromList [1..3]
-- Nothing
--
findIndex :: V.Unbox a => (a -> Bool) -> NonEmptyUVector a -> Maybe Int
findIndex f = V.findIndex f . _neUvec
{-# INLINE findIndex #-}

-- | /O(n)/ Yield the indices of elements satisfying the predicate in
-- ascending order.
--
-- >>> findIndices (< 3) $ unsafeFromList [1..3]
-- [0,1]
--
-- >>> findIndices (< 0) $ unsafeFromList [1..3]
-- []
--
findIndices :: V.Unbox a => (a -> Bool) -> NonEmptyUVector a -> Vector Int
findIndices f = V.findIndices f . _neUvec
{-# INLINE findIndices #-}

-- | /O(n)/ Yield 'Just' the index of the first occurence of the given
-- element or 'Nothing' if the non-empty vector does not contain the
-- element. This is a specialised version of 'findIndex'.
--
-- >>> elemIndex 1 $ unsafeFromList [1..3]
-- Just 0
--
-- >>> elemIndex 0 $ unsafeFromList [1..3]
-- Nothing
--
elemIndex :: (V.Unbox a, Eq a) => a -> NonEmptyUVector a -> Maybe Int
elemIndex a = V.elemIndex a . _neUvec
{-# INLINE elemIndex #-}

-- | /O(n)/ Yield the indices of all occurences of the given element in
-- ascending order. This is a specialised version of 'findIndices'.
--
-- >>> elemIndices 1 $ unsafeFromList [1,2,3,1]
-- [0,3]
--
-- >>> elemIndices 0 $ unsafeFromList [1..3]
-- []
--
elemIndices :: (V.Unbox a, Eq a) => a -> NonEmptyUVector a -> Vector Int
elemIndices a = V.elemIndices a . _neUvec
{-# INLINE elemIndices #-}

-- ---------------------------------------------------------------------- --
-- Folding

-- | /O(n)/ Left monoidal fold
--
foldl :: V.Unbox b => (a -> b -> a) -> a -> NonEmptyUVector b -> a
foldl f a = V.foldl f a . _neUvec
{-# INLINE foldl #-}

-- | /O(n)/ Left semigroupal fold
--
foldl1 :: V.Unbox a => (a -> a -> a) -> NonEmptyUVector a -> a
foldl1 f = V.foldl1 f . _neUvec
{-# INLINE foldl1 #-}

-- | /O(n)/ Strict Left monoidal fold
--
foldl' :: V.Unbox b => (a -> b -> a) -> a -> NonEmptyUVector b -> a
foldl' f a = V.foldl' f a . _neUvec
{-# INLINE foldl' #-}

-- | /O(n)/ Strict Left semigroupal fold
--
foldl1' :: V.Unbox a => (a -> a -> a) -> NonEmptyUVector a -> a
foldl1' f = V.foldl1' f . _neUvec
{-# INLINE foldl1' #-}

-- | /O(n)/ Right monoidal fold
--
foldr :: V.Unbox a => (a -> b -> b) -> b -> NonEmptyUVector a -> b
foldr f b = V.foldr f b . _neUvec
{-# INLINE foldr #-}

-- | /O(n)/ Right semigroupal fold
--
foldr1 :: V.Unbox a => (a -> a -> a) -> NonEmptyUVector a -> a
foldr1 f = V.foldr1 f . _neUvec
{-# INLINE foldr1 #-}

-- | /O(n)/ Strict right monoidal fold
--
foldr' :: V.Unbox a => (a -> b -> b) -> b -> NonEmptyUVector a -> b
foldr' f b = V.foldr' f b. _neUvec
{-# INLINE foldr' #-}

-- | /O(n)/ Strict right semigroupal fold
--
foldr1' :: V.Unbox a => (a -> a -> a) -> NonEmptyUVector a -> a
foldr1' f = V.foldr1' f . _neUvec
{-# INLINE foldr1' #-}

-- | /O(n)/ Left monoidal fold with function applied to each element
-- and its index
--
ifoldl :: V.Unbox b => (a -> Int -> b -> a) -> a -> NonEmptyUVector b -> a
ifoldl f a = V.ifoldl f a . _neUvec
{-# INLINE ifoldl #-}

-- | /O(n)/ Strict left monoidal fold with function applied to each element
-- and its index
--
ifoldl' :: V.Unbox b => (a -> Int -> b -> a) -> a -> NonEmptyUVector b -> a
ifoldl' f a = V.ifoldl' f a . _neUvec
{-# INLINE ifoldl' #-}

-- | /O(n)/ Right monoidal fold with function applied to each element
-- and its index
--
ifoldr :: V.Unbox a => (Int -> a -> b -> b) -> b -> NonEmptyUVector a -> b
ifoldr f b = V.ifoldr f b . _neUvec
{-# INLINE ifoldr #-}

-- | /O(n)/ strict right monoidal fold with function applied to each element
-- and its index
--
ifoldr' :: V.Unbox a => (Int -> a -> b -> b) -> b -> NonEmptyUVector a -> b
ifoldr' f b = V.ifoldr' f b . _neUvec
{-# INLINE ifoldr' #-}

-- ---------------------------------------------------------------------- --
-- Specialised folds

-- | /O(n)/ Check if all elements satisfy the predicate.
--
all :: V.Unbox a => (a -> Bool) -> NonEmptyUVector a -> Bool
all f = V.all f . _neUvec
{-# INLINE all #-}

-- | /O(n)/ Check if any element satisfies the predicate.
--
any :: V.Unbox a => (a -> Bool) -> NonEmptyUVector a -> Bool
any f = V.any f . _neUvec
{-# INLINE any #-}

-- | /O(n)/ Check if all elements are @True@.
--
and :: NonEmptyUVector Bool -> Bool
and = V.and . _neUvec
{-# INLINE and #-}

-- | /O(n)/ Check if any element is @True@
--
or :: NonEmptyUVector Bool -> Bool
or = V.or . _neUvec
{-# INLINE or #-}

-- | /O(n)/ Compute the sum of the elements
--
sum :: (Num a, V.Unbox a) => NonEmptyUVector a -> a
sum = V.sum . _neUvec
{-# INLINE sum #-}

-- | /O(n)/ Compute the produce of the elements
--
product :: (Num a, V.Unbox a) => NonEmptyUVector a -> a
product = V.product . _neUvec
{-# INLINE product #-}

-- | /O(n)/ Yield the maximum element of the non-empty vector.
--
maximum :: (V.Unbox a, Ord a) => NonEmptyUVector a -> a
maximum = V.maximum . _neUvec
{-# INLINE maximum #-}

-- | /O(n)/ Yield the maximum element of a non-empty vector
-- according to the given comparison function.
--
maximumBy :: V.Unbox a => (a -> a -> Ordering) -> NonEmptyUVector a -> a
maximumBy f = V.maximumBy f . _neUvec
{-# INLINE maximumBy #-}

-- | /O(n)/ Yield the minimum element of the non-empty vector.
--
minimum :: (V.Unbox a, Ord a) => NonEmptyUVector a -> a
minimum = V.minimum . _neUvec
{-# INLINE minimum #-}

-- | /O(n)/ Yield the minimum element of the non-empty vector
-- according to the given comparison function.
--
minimumBy :: V.Unbox a => (a -> a -> Ordering) -> NonEmptyUVector a -> a
minimumBy f = V.minimumBy f . _neUvec
{-# INLINE minimumBy #-}

-- | /O(n)/ Yield the index of the minimum element of the
-- non-empty vector.
--
minIndex :: (V.Unbox a, Ord a) => NonEmptyUVector a -> Int
minIndex = V.minIndex . _neUvec
{-# INLINE minIndex #-}

-- | /O(n)/ Yield the index of the minimum element of the vector
-- according to the given comparison function.
--
minIndexBy :: V.Unbox a => (a -> a -> Ordering) -> NonEmptyUVector a -> Int
minIndexBy f = V.minIndexBy f . _neUvec
{-# INLINE minIndexBy #-}

-- | /O(n)/ Yield the index of the maximum element of the
-- non-empty vector.
--
maxIndex :: (V.Unbox a, Ord a) => NonEmptyUVector a -> Int
maxIndex = V.maxIndex . _neUvec
{-# INLINE maxIndex #-}

-- | /O(n)/ Yield the index of the maximum element of the vector
-- according to the given comparison function.
--
maxIndexBy :: V.Unbox a => (a -> a -> Ordering) -> NonEmptyUVector a -> Int
maxIndexBy f = V.maxIndexBy f . _neUvec
{-# INLINE maxIndexBy #-}

-- ---------------------------------------------------------------------- --
-- Monadic folds

-- | /O(n)/ Monadic fold
--
foldM :: (V.Unbox b, Monad m) => (a -> b -> m a) -> a -> NonEmptyUVector b -> m a
foldM f a = V.foldM f a . _neUvec
{-# INLINE foldM #-}

-- | /O(n)/ Monadic fold (action applied to each element and its index)
--
ifoldM :: (V.Unbox b, Monad m) => (a -> Int -> b -> m a) -> a -> NonEmptyUVector b -> m a
ifoldM f a = V.ifoldM f a . _neUvec
{-# INLINE ifoldM #-}

-- | /O(n)/ Strict monadic fold
--
foldM' :: (V.Unbox b, Monad m) => (a -> b -> m a) -> a -> NonEmptyUVector b -> m a
foldM' f a = V.foldM' f a . _neUvec
{-# INLINE foldM' #-}

-- | /O(n)/ Strict monadic fold (action applied to each element and its index)
--
ifoldM' :: (V.Unbox b, Monad m) => (a -> Int -> b -> m a) -> a -> NonEmptyUVector b -> m a
ifoldM' f a = V.ifoldM' f a . _neUvec
{-# INLINE ifoldM' #-}

-- | /O(n)/ Monadic semigroupal fold
--
fold1M :: (V.Unbox a, Monad m) => (a -> a -> m a) -> NonEmptyUVector a -> m a
fold1M f = V.fold1M f . _neUvec
{-# INLINE fold1M #-}

-- | /O(n)/ Strict monadic semigroupal fold
--
fold1M' :: (V.Unbox a, Monad m) => (a -> a -> m a) -> NonEmptyUVector a -> m a
fold1M' f = V.fold1M' f . _neUvec
{-# INLINE fold1M' #-}

-- | /O(n)/ Monadic fold that discards the result
--
foldM_ :: (V.Unbox b, Monad m) => (a -> b -> m a) -> a -> NonEmptyUVector b -> m ()
foldM_ f a = V.foldM_ f a . _neUvec
{-# INLINE foldM_ #-}

-- | /O(n)/ Monadic fold that discards the result (action applied to each
-- element and its index)
--
ifoldM_ :: (V.Unbox b, Monad m) => (a -> Int -> b -> m a) -> a -> NonEmptyUVector b -> m ()
ifoldM_ f a = V.ifoldM_ f a . _neUvec
{-# INLINE ifoldM_ #-}

-- | /O(n)/ Strict monadic fold that discards the result
--
foldM'_ :: (V.Unbox b, Monad m) => (a -> b -> m a) -> a -> NonEmptyUVector b -> m ()
foldM'_ f a = V.foldM'_ f a . _neUvec
{-# INLINE foldM'_ #-}

-- | /O(n)/ Strict monadic fold that discards the result (action applied to each
-- element and its index)
--
ifoldM'_ :: (V.Unbox b, Monad m) => (a -> Int -> b -> m a) -> a -> NonEmptyUVector b -> m ()
ifoldM'_ f a = V.ifoldM'_ f a . _neUvec
{-# INLINE ifoldM'_ #-}

-- | /O(n)/ Monadic semigroupal fold that discards the result
--
fold1M_ :: (V.Unbox a, Monad m) => (a -> a -> m a) -> NonEmptyUVector a -> m ()
fold1M_ f = V.fold1M_ f . _neUvec
{-# INLINE fold1M_ #-}

-- | /O(n)/ Strict monadic semigroupal fold that discards the result
--
fold1M'_ :: (V.Unbox a, Monad m) => (a -> a -> m a) -> NonEmptyUVector a -> m ()
fold1M'_ f = V.fold1M'_ f . _neUvec
{-# INLINE fold1M'_ #-}

-- ---------------------------------------------------------------------- --
-- Prefix sums (scans)

-- | /O(n)/ Prescan
--
prescanl :: (V.Unbox a, V.Unbox b) => (a -> b -> a) -> a -> NonEmptyUVector b -> NonEmptyUVector a
prescanl f a = NonEmptyUVector . V.prescanl f a . _neUvec
{-# INLINE prescanl #-}

-- | /O(n)/ Prescan with strict accumulator
--
prescanl' :: (V.Unbox a, V.Unbox b) => (a -> b -> a) -> a -> NonEmptyUVector b -> NonEmptyUVector a
prescanl' f a = NonEmptyUVector . V.prescanl' f a . _neUvec
{-# INLINE prescanl' #-}

-- | /O(n)/ Scan
--
postscanl :: (V.Unbox a, V.Unbox b) => (a -> b -> a) -> a -> NonEmptyUVector b -> NonEmptyUVector a
postscanl f a = NonEmptyUVector . V.postscanl f a . _neUvec
{-# INLINE postscanl #-}

-- | /O(n)/ Scan with a strict accumulator
--
postscanl' :: (V.Unbox a, V.Unbox b) => (a -> b -> a) -> a -> NonEmptyUVector b -> NonEmptyUVector a
postscanl' f a = NonEmptyUVector . V.postscanl' f a . _neUvec
{-# INLINE postscanl' #-}

-- | /O(n)/ Haskell-style scan
--
scanl :: (V.Unbox a, V.Unbox b) => (a -> b -> a) -> a -> NonEmptyUVector b -> NonEmptyUVector a
scanl f a = NonEmptyUVector . V.scanl f a . _neUvec
{-# INLINE scanl #-}

-- | /O(n)/ Haskell-style scan with strict accumulator
--
scanl' :: (V.Unbox a, V.Unbox b) => (a -> b -> a) -> a -> NonEmptyUVector b -> NonEmptyUVector a
scanl' f a = NonEmptyUVector . V.scanl' f a . _neUvec
{-# INLINE scanl' #-}

-- | /O(n)/ Semigroupal left scan
--
scanl1 :: V.Unbox a => (a -> a -> a) -> NonEmptyUVector a -> NonEmptyUVector a
scanl1 f = NonEmptyUVector . V.scanl1 f . _neUvec
{-# INLINE scanl1 #-}

-- | /O(n)/ Strict semigroupal scan
--
scanl1' :: V.Unbox a => (a -> a -> a) -> NonEmptyUVector a -> NonEmptyUVector a
scanl1' f = NonEmptyUVector . V.scanl1' f . _neUvec
{-# INLINE scanl1' #-}

-- | /O(n)/ Scan over a vector with its index
--
iscanl :: (V.Unbox a, V.Unbox b) => (Int -> a -> b -> a) -> a -> NonEmptyUVector b -> NonEmptyUVector a
iscanl f a = NonEmptyUVector . V.iscanl f a . _neUvec
{-# INLINE iscanl #-}

-- | /O(n)/ Scan over a vector with its index with strict accumulator
--
iscanl' :: (V.Unbox a, V.Unbox b) => (Int -> a -> b -> a) -> a -> NonEmptyUVector b -> NonEmptyUVector a
iscanl' f a = NonEmptyUVector . V.iscanl' f a . _neUvec
{-# INLINE iscanl' #-}

-- | /O(n)/ Right-to-left prescan
--
prescanr :: (V.Unbox a, V.Unbox b) => (a -> b -> b) -> b -> NonEmptyUVector a -> NonEmptyUVector b
prescanr f b = NonEmptyUVector . V.prescanr f b . _neUvec
{-# INLINE prescanr #-}

-- | /O(n)/ Right-to-left prescan with strict accumulator
--
prescanr' :: (V.Unbox a, V.Unbox b) => (a -> b -> b) -> b -> NonEmptyUVector a -> NonEmptyUVector b
prescanr' f b = NonEmptyUVector . V.prescanr' f b . _neUvec
{-# INLINE prescanr' #-}

-- | /O(n)/ Right-to-left scan
--
postscanr :: (V.Unbox a, V.Unbox b) => (a -> b -> b) -> b -> NonEmptyUVector a -> NonEmptyUVector b
postscanr f b = NonEmptyUVector . V.postscanr f b . _neUvec
{-# INLINE postscanr #-}

-- | /O(n)/ Right-to-left scan with strict accumulator
--
postscanr' :: (V.Unbox a, V.Unbox b) => (a -> b -> b) -> b -> NonEmptyUVector a -> NonEmptyUVector b
postscanr' f b = NonEmptyUVector . V.postscanr' f b . _neUvec
{-# INLINE postscanr' #-}

-- | /O(n)/ Right-to-left Haskell-style scan
--
scanr :: (V.Unbox a, V.Unbox b) => (a -> b -> b) -> b -> NonEmptyUVector a -> NonEmptyUVector b
scanr f b = NonEmptyUVector . V.scanr f b . _neUvec
{-# INLINE scanr #-}

-- | /O(n)/ Right-to-left Haskell-style scan with strict accumulator
--
scanr' :: (V.Unbox a, V.Unbox b) => (a -> b -> b) -> b -> NonEmptyUVector a -> NonEmptyUVector b
scanr' f b = NonEmptyUVector . V.scanr' f b . _neUvec
{-# INLINE scanr' #-}

-- | /O(n)/ Right-to-left Haskell-style semigroupal scan
--
scanr1 :: V.Unbox a => (a -> a -> a) -> NonEmptyUVector a -> NonEmptyUVector a
scanr1 f = NonEmptyUVector . V.scanr1 f . _neUvec
{-# INLINE scanr1 #-}

-- | /O(n)/ Right-to-left Haskell-style semigroupal scan with strict accumulator
--
scanr1' :: V.Unbox a => (a -> a -> a) -> NonEmptyUVector a -> NonEmptyUVector a
scanr1' f = NonEmptyUVector . V.scanr1' f . _neUvec
{-# INLINE scanr1' #-}

-- | /O(n)/ Right-to-left scan over a vector with its index
--
iscanr :: (V.Unbox a, V.Unbox b) => (Int -> a -> b -> b) -> b -> NonEmptyUVector a -> NonEmptyUVector b
iscanr f b = NonEmptyUVector . V.iscanr f b . _neUvec
{-# INLINE iscanr #-}

-- | /O(n)/ Right-to-left scan over a vector with its index and a strict
-- accumulator
--
iscanr' :: (V.Unbox a, V.Unbox b) => (Int -> a -> b -> b) -> b -> NonEmptyUVector a -> NonEmptyUVector b
iscanr' f b = NonEmptyUVector . V.iscanr' f b . _neUvec
{-# INLINE iscanr' #-}

-- | /O(n)/ The 'intersperse' function takes an element and a 'NonEmptyUVector'
-- and "intersperses" that element between the elements of the 'NonEmptyUVector'.
--
-- >>> intersperse 0 (unsafeFromList [1,2,3])
-- [1,0,2,0,3]
--
-- >>> intersperse 0 (singleton 1)
-- [1]
--
intersperse :: forall (a :: Type) . V.Unbox a => a -> NonEmptyUVector a -> NonEmptyUVector a
intersperse sep nev@(NonEmptyUVector v)
    | V.length v <= 1 = nev
    | otherwise = unsafeCreate $ do
        let n = V.length v
        let newLen = 2*n - 1
        mv <- V.unsafeThaw $ V.replicate newLen (V.unsafeHead v)  -- Create mutable vector

        -- Fill the first element
        unsafeWrite mv 0 (V.unsafeIndex v 0)

        -- Fill remaining elements with separators
        let go i j
              | j >= n    = pure ()
              | otherwise = do
                  unsafeWrite mv i sep
                  unsafeWrite mv (i+1) (V.unsafeIndex v j)
                  go (i+2) (j+1)

        go 1 1
        pure mv
{-# INLINE intersperse #-}

-- | /O(n)/ Split a vector into a list of slices of the input vector.
--
-- The concatenation of this list of slices is equal to the argument vector,
-- and each slice contains only equal elements. Returns nothing if no equal
-- elements are found.
--
-- This is the equivalent of 'groupBy (==)'.
--
-- >>> import qualified Data.NonEmptyUVector as NEV
-- >>> NEV.group (NEV.unsafeFromList "Mississippi")
-- "M" :| ["i","ss","i","ss","i","pp","i"]
--
-- See also 'Data.Vector.group'.
--
-- @since 0.3
--
group :: (V.Unbox a, Eq a) => NonEmptyUVector a -> NonEmpty (NonEmptyUVector a)
group = groupBy (==)

-- | /O(n)/ Split a vector into a list of slices.
--
--
-- The concatenation of this list of slices is equal to the argument vector,
-- and each slice contains only equal elements, as determined by the equality
-- predicate function.
--

-- >>> import qualified Data.NonEmptyUVector as NEV
-- >>> import Data.Char (isUpper)
-- >>> NEV.groupBy (\a b -> isUpper a == isUpper b) (NEV.unsafeFromList "Mississippi River")
-- "M" :| ["ississippi ","R","iver"]
--
-- See also 'Data.Vector.groupBy'.
--
-- @since 0.3
--
groupBy
    :: (Eq a, V.Unbox a)
    => (a -> a -> Bool)
    -> NonEmptyUVector a
    -> NonEmpty (NonEmptyUVector a)
groupBy f = go
  where
    go v =
      let
        h = head v
        t = NonEmptyUVector $ tail v
      in case findIndex (not . f h) t of
           Nothing -> v :| []
           Just n ->
             let
               z = NonEmptyUVector $ unsafeTake (n+1) v
               x = NonEmptyUVector $ unsafeDrop (n+1) v
             in z <| go x
