{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
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

  -- ** Length information
, length

  -- ** Indexing
, head, last, (!), (!?)
, unsafeIndex

  -- ** Monadic Indexing
, headM, lastM, indexM, unsafeIndexM

  -- ** Extracting subvectors (slicing)
, tail, slice, init, take, drop, splitAt
, unsafeSlice, unsafeTake, unsafeDrop

  -- * Construction

  -- ** Initialization
, singleton, replicate, generate
, iterateN

  -- ** Monad Initialization
, replicateM, generateM, iterateNM

  -- ** Unfolding
, unfoldr, unfoldrN, unfoldrM, unfoldrNM
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
, fromNonEmpty, toNonEmpty

  -- ** To/from vector
, toVector, fromVector

  -- ** From list
, fromList

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

) where


import Prelude (Bool, Eq, Ord, Read, Show, Num, Enum, (.))


import Control.Applicative
import Control.DeepSeq hiding (force)
import Control.Monad (Monad)
import Control.Monad.Fail
import Control.Monad.ST
import Control.Monad.Zip (MonadZip)

import Data.Data (Data)
import Data.Foldable (Foldable, toList, foldl', foldMap)
import Data.Functor
import Data.Int
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (Maybe(..))
import Data.Semigroup (Semigroup(..), (<>))
import Data.Traversable (Traversable, traverse)
import Data.Typeable (Typeable)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as MV
import GHC.Generics


newtype NonEmptyVector a = NonEmptyVector
    { _neVec :: V.Vector a
    } deriving
      ( Eq, Ord, Show, Read
      , Data, Typeable, Generic, NFData
      , Functor, Applicative, Monad
      , MonadFail, MonadZip, Alternative
      , Semigroup
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

unsafeDrop :: Int -> NonEmptyVector a -> Vector a
unsafeDrop n = V.unsafeDrop n . _neVec

-- ---------------------------------------------------------------------- --
-- Construction

singleton :: a -> NonEmptyVector a
singleton = NonEmptyVector . V.singleton
{-# INLINE singleton #-}

replicate :: Int -> a -> Maybe (NonEmptyVector a)
replicate n a = fromVector (V.replicate n a)
{-# INLINE replicate #-}

generate :: Int -> (Int -> a) -> Maybe (NonEmptyVector a)
generate n f = fromVector (V.generate n f)
{-# INLINE generate #-}

iterateN :: Int -> (a -> a) -> a -> Maybe (NonEmptyVector a)
iterateN n f a = fromVector (V.iterateN n f a)
{-# INLINE iterateN #-}

-- ---------------------------------------------------------------------- --
-- Monadic Initialization

replicateM :: Monad m => Int -> m a -> m (Maybe (NonEmptyVector a))
replicateM n a = fmap fromVector (V.replicateM n a)
{-# INLINE replicateM #-}

generateM :: Monad m => Int -> (Int -> m a) -> m (Maybe (NonEmptyVector a))
generateM n f = fmap fromVector (V.generateM n f)
{-# INLINE generateM #-}

iterateNM :: Monad m => Int -> (a -> m a) -> a -> m (Maybe (NonEmptyVector a))
iterateNM n f a = fmap fromVector (V.iterateNM n f a)
{-# INLINE iterateNM #-}

-- ---------------------------------------------------------------------- --
-- Unfolding

unfoldr :: (b -> Maybe (a, b)) -> b -> Maybe (NonEmptyVector a)
unfoldr f b = fromVector (V.unfoldr f b)

unfoldrN :: Int -> (b -> Maybe (a, b)) -> b -> Maybe (NonEmptyVector a)
unfoldrN n f b = fromVector (V.unfoldrN n f b)

unfoldrM :: Monad m => (b -> m (Maybe (a, b))) -> b -> m (Maybe (NonEmptyVector a))
unfoldrM f b = fmap fromVector (V.unfoldrM f b)

unfoldrNM :: Monad m => Int -> (b -> m (Maybe (a, b))) -> b -> m (Maybe (NonEmptyVector a))
unfoldrNM n f b = fmap fromVector (V.unfoldrNM n f b)

constructN :: Int -> (Vector a -> a) -> Maybe (NonEmptyVector a)
constructN n f = fromVector (V.constructN n f)

constructrN :: Int -> (Vector a -> a) -> Maybe (NonEmptyVector a)
constructrN n f = fromVector (V.constructrN n f)

-- ---------------------------------------------------------------------- --
-- Enumeration

enumFromN :: Num a => a -> Int -> Maybe (NonEmptyVector a)
enumFromN a n = fromVector (V.enumFromN a n)

enumFromStepN :: Num a => a -> a -> Int -> Maybe (NonEmptyVector a)
enumFromStepN a0 a1 n = fromVector (V.enumFromStepN a0 a1 n)

enumFromTo :: Enum a => a -> a -> Maybe (NonEmptyVector a)
enumFromTo a0 a1 = fromVector (V.enumFromTo a0 a1)

enumFromThenTo :: Enum a => a -> a -> a -> Maybe (NonEmptyVector a)
enumFromThenTo a0 a1 a2 = fromVector (V.enumFromThenTo a0 a1 a2)

-- ---------------------------------------------------------------------- --
-- Concatenation

cons :: a -> NonEmptyVector a -> NonEmptyVector a
cons a (NonEmptyVector as) = NonEmptyVector (V.cons a as)
{-# INLINE cons #-}

snoc :: NonEmptyVector a -> a -> NonEmptyVector a
snoc (NonEmptyVector as) a = NonEmptyVector (V.snoc as a)
{-# INLINE snoc #-}

(++) :: NonEmptyVector a -> NonEmptyVector a -> NonEmptyVector a
NonEmptyVector v ++ NonEmptyVector v' = NonEmptyVector (v <> v')
{-# INLINE (++) #-}

concat :: [NonEmptyVector a] -> Maybe (NonEmptyVector a)
concat [] = Nothing
concat (a:as) = Just (concat1 (a :| as))

concat1 :: NonEmpty (NonEmptyVector a) -> NonEmptyVector a
concat1 = NonEmptyVector . foldl' (\v (NonEmptyVector a) -> v <> a) V.empty

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

fromList :: [a] -> Maybe (NonEmptyVector a)
fromList = fromVector . V.fromList
{-# INLINE fromList #-}

-- ---------------------------------------------------------------------- --
-- Restricting memory usage

force :: NonEmptyVector a -> NonEmptyVector a
force (NonEmptyVector a) = NonEmptyVector (V.force a)

-- ---------------------------------------------------------------------- --
-- Bulk Updates

(//) :: NonEmptyVector a -> [(Int, a)] -> NonEmptyVector a
NonEmptyVector v // us = NonEmptyVector (v V.// us)

update :: NonEmptyVector a -> Vector (Int, a) -> NonEmptyVector a
update (NonEmptyVector v) v' = NonEmptyVector (V.update v v')

update_ :: NonEmptyVector a -> Vector Int -> Vector a -> NonEmptyVector a
update_ (NonEmptyVector v) is as = NonEmptyVector (V.update_ v is as)

unsafeUpd :: NonEmptyVector a -> [(Int, a)] -> NonEmptyVector a
unsafeUpd (NonEmptyVector v) us = NonEmptyVector (V.unsafeUpd v us)

unsafeUpdate :: NonEmptyVector a -> Vector (Int, a) -> NonEmptyVector a
unsafeUpdate (NonEmptyVector v) us = NonEmptyVector (V.unsafeUpdate v us)

unsafeUpdate_ :: NonEmptyVector a -> Vector Int -> Vector a -> NonEmptyVector a
unsafeUpdate_ (NonEmptyVector v) is as = NonEmptyVector (V.unsafeUpdate_ v is as)

-- ---------------------------------------------------------------------- --
-- Accumulation

accum
    :: (a -> b -> a)
    -> NonEmptyVector a
    -> [(Int, b)]
    -> NonEmptyVector a
accum f (NonEmptyVector v) us = NonEmptyVector (V.accum f v us)

accumulate
    :: (a -> b -> a)
    -> NonEmptyVector a
    -> Vector (Int, b)
    -> NonEmptyVector a
accumulate f (NonEmptyVector v) us = NonEmptyVector (V.accumulate f v us)

accumulate_
    :: (a -> b -> a)
    -> NonEmptyVector a
    -> Vector Int
    -> Vector b
    -> NonEmptyVector a
accumulate_ f (NonEmptyVector v) is bs
    = NonEmptyVector (V.accumulate_ f v is bs)

unsafeAccum
    :: (a -> b -> a)
    -> NonEmptyVector a
    -> [(Int, b)]
    -> NonEmptyVector a
unsafeAccum f (NonEmptyVector v) us = NonEmptyVector (V.unsafeAccum f v us)

unsafeAccumulate
    :: (a -> b -> a)
    -> NonEmptyVector a
    -> Vector (Int, b)
    -> NonEmptyVector a
unsafeAccumulate f (NonEmptyVector v) us
    = NonEmptyVector (V.unsafeAccumulate f v us)

unsafeAccumulate_
    :: (a -> b -> a)
    -> NonEmptyVector a
    -> Vector Int
    -> Vector b
    -> NonEmptyVector a
unsafeAccumulate_ f (NonEmptyVector v) is bs
    = NonEmptyVector (V.unsafeAccumulate_ f v is bs)

-- ---------------------------------------------------------------------- --
-- Permutations

reverse :: NonEmptyVector a -> NonEmptyVector a
reverse = NonEmptyVector . V.reverse . _neVec

backpermute :: NonEmptyVector a -> NonEmptyVector Int -> NonEmptyVector a
backpermute (NonEmptyVector v) (NonEmptyVector i)
    = NonEmptyVector (V.backpermute v i)

unsafeBackpermute
    :: NonEmptyVector a
    -> NonEmptyVector Int
    -> NonEmptyVector a
unsafeBackpermute (NonEmptyVector v) (NonEmptyVector i)
    = NonEmptyVector (V.unsafeBackpermute v i)

-- ---------------------------------------------------------------------- --
-- Safe destructive updates

modify
    :: (forall s. MVector s a -> ST s ())
    -> NonEmptyVector a
    -> NonEmptyVector a
modify p (NonEmptyVector v) = NonEmptyVector (V.modify p v)

-- ---------------------------------------------------------------------- --
-- Indexing

indexed :: NonEmptyVector a -> NonEmptyVector (Int, a)
indexed = NonEmptyVector . V.indexed . _neVec

-- ---------------------------------------------------------------------- --
-- Mapping

map :: (a -> b) -> NonEmptyVector a -> NonEmptyVector b
map f = NonEmptyVector . V.map f . _neVec

imap :: (Int -> a -> b) -> NonEmptyVector a -> NonEmptyVector b
imap f = NonEmptyVector . V.imap f . _neVec

concatMap
    :: (a -> NonEmptyVector b)
    -> NonEmptyVector a
    -> NonEmptyVector b
concatMap f = NonEmptyVector . V.concatMap (_neVec . f) . _neVec

-- ---------------------------------------------------------------------- --
-- Monadic Mapping

mapM :: Monad m => (a -> m b) -> NonEmptyVector a -> m (NonEmptyVector b)
mapM f = fmap NonEmptyVector . V.mapM f . _neVec

imapM
    :: Monad m
    => (Int -> a -> m b)
    -> NonEmptyVector a
    -> m (NonEmptyVector b)
imapM f = fmap NonEmptyVector . V.imapM f . _neVec

mapM_ :: Monad m => (a -> m b) -> NonEmptyVector a -> m ()
mapM_ f = V.mapM_ f . _neVec

imapM_ :: Monad m => (Int -> a -> m b) -> NonEmptyVector a -> m ()
imapM_ f = V.imapM_ f . _neVec

forM :: Monad m => NonEmptyVector a -> (a -> m b) -> m (NonEmptyVector b)
forM (NonEmptyVector v) f = fmap NonEmptyVector (V.forM v f)

forM_ :: Monad m => NonEmptyVector a -> (a -> m b) -> m ()
forM_ (NonEmptyVector v) f = V.forM_ v f

-- ---------------------------------------------------------------------- --
-- Zipping

zipWith
    :: (a -> b -> c)
    -> NonEmptyVector a
    -> NonEmptyVector b
    -> NonEmptyVector c
zipWith f a b = NonEmptyVector (V.zipWith f a' b')
  where
    a' = _neVec a
    b' = _neVec b

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

izipWith
    :: (Int -> a -> b -> c)
    -> NonEmptyVector a
    -> NonEmptyVector b
    -> NonEmptyVector c
izipWith f a b = NonEmptyVector (V.izipWith f a' b')
  where
    a' = _neVec a
    b' = _neVec b

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

zip :: NonEmptyVector a -> NonEmptyVector b -> NonEmptyVector (a, b)
zip a b = NonEmptyVector (V.zip a' b')
  where
    a' = _neVec a
    b' = _neVec b

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

-- ---------------------------------------------------------------------- --
-- Monadic Zipping

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

zipWithM_
    :: Monad m
    => (a -> b -> m c)
    -> NonEmptyVector a
    -> NonEmptyVector b
    -> m ()
zipWithM_ f a b = V.zipWithM_ f (_neVec a) (_neVec b)


izipWithM_
    :: Monad m
    => (Int -> a -> b -> m c)
    -> NonEmptyVector a
    -> NonEmptyVector b
    -> m ()
izipWithM_ f a b = V.izipWithM_ f (_neVec a) (_neVec b)

-- ---------------------------------------------------------------------- --
-- Unzipping

unzip :: NonEmptyVector (a, b) -> (NonEmptyVector a, NonEmptyVector b)
unzip (NonEmptyVector v) = case V.unzip v of
    ~(a,b) -> (NonEmptyVector a, NonEmptyVector b)

unzip3
    :: NonEmptyVector (a, b, c)
    -> (NonEmptyVector a, NonEmptyVector b, NonEmptyVector c)
unzip3 (NonEmptyVector v) = case V.unzip3 v of
    ~(a,b,c) ->
      ( NonEmptyVector a
      , NonEmptyVector b
      , NonEmptyVector c
      )

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

-- ---------------------------------------------------------------------- --
-- Filtering

filter :: (a -> Bool) -> NonEmptyVector a -> Vector a
filter f = V.filter f . _neVec

ifilter
    :: (Int -> a -> Bool)
    -> NonEmptyVector a
    -> Vector a
ifilter f = V.ifilter f . _neVec

uniq :: Eq a => NonEmptyVector a -> NonEmptyVector a
uniq = NonEmptyVector . V.uniq . _neVec

mapMaybe
    :: (a -> Maybe b)
    -> NonEmptyVector a
    -> Vector b
mapMaybe f = V.mapMaybe f . _neVec

imapMaybe
    :: (Int -> a -> Maybe b)
    -> NonEmptyVector a
    -> Vector b
imapMaybe f = V.imapMaybe f . _neVec

filterM
    :: Monad m
    => (a -> m Bool)
    -> NonEmptyVector a
    -> m (Vector a)
filterM f = V.filterM f . _neVec

takeWhile :: (a -> Bool) -> NonEmptyVector a -> Vector a
takeWhile f = V.takeWhile f . _neVec

dropWhile :: (a -> Bool) -> NonEmptyVector a -> Vector a
dropWhile f = V.dropWhile f . _neVec

-- ---------------------------------------------------------------------- --
-- Partitioning

partition :: (a -> Bool) -> NonEmptyVector a -> (Vector a, Vector a)
partition f = V.partition f . _neVec

unstablePartition
    :: (a -> Bool)
    -> NonEmptyVector a
    -> (Vector a, Vector a)
unstablePartition f = V.unstablePartition f . _neVec

span :: (a -> Bool) -> NonEmptyVector a -> (Vector a, Vector a)
span f = V.span f . _neVec

break :: (a -> Bool) -> NonEmptyVector a -> (Vector a, Vector a)
break f = V.break f . _neVec
