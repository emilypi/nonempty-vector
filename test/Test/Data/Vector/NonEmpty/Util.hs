{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- |
-- Module      : Test.Data.Vector.NonEmpty.Util
-- Copyright   : (c) 2019-2026 Emily Pillmore, Koz Ross
-- License     : BSD-style
--
-- Maintainer  : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability   : experimental
-- Portability : non-portable
--
-- Generators, list models, and property helpers shared by the
-- non-empty vector test suite.
--
-- Vectors are built and observed through the representation exposed by
-- "Data.Vector.NonEmpty.Internal", so that the models never depend on
-- the API under test.
--
module Test.Data.Vector.NonEmpty.Util
( -- * Building and observing vectors
  vec
, vecU
, elems
, elemsU
, elems1
, elems1U
, whenNonEmpty
  -- * Generators
, genIndex
, genIndexU
, genIndices
, genIndicesU
, genOutOfBounds
, genOutOfBoundsU
, genSlice
, genSliceU
, genUpdates
, genUpdatesU
  -- * Effects
, logged
, unfoldStep
  -- * List models
, adjustAt
, lookupList
, swapAt
, unfoldrMList
, unfoldrNMList
  -- * Exceptions
, throws
, throwsIO
) where


import Control.DeepSeq (NFData, force)
import Control.Exception (SomeException, evaluate, try)

import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Vector.NonEmpty.Internal (NonEmptyVector(..))
import Data.Vector.NonEmpty.Unboxed.Internal (NonEmptyUVector(..))

import Test.QuickCheck


instance Arbitrary a => Arbitrary (NonEmptyVector a) where
    arbitrary = vec . getNonEmpty <$> arbitrary
    shrink = fmap (vec . getNonEmpty) . shrink . NonEmpty . elems

instance (Arbitrary a, VU.Unbox a) => Arbitrary (NonEmptyUVector a) where
    arbitrary = vecU . getNonEmpty <$> arbitrary
    shrink = fmap (vecU . getNonEmpty) . shrink . NonEmpty . elemsU

-- ---------------------------------------------------------------------- --
-- Building and observing vectors

-- | Build a non-empty vector from its representation. The caller
-- guarantees that the list is non-empty.
--
vec :: [a] -> NonEmptyVector a
vec = NonEmptyVector . V.fromList

-- | As 'vec', but for unboxed representations.
--
vecU :: VU.Unbox a => [a] -> NonEmptyUVector a
vecU = NonEmptyUVector . VU.fromList

-- | The elements of a non-empty vector, read from its representation.
--
elems :: NonEmptyVector a -> [a]
elems = V.toList . _neVec

-- | As 'elems', but for unboxed representations.
--
elemsU :: VU.Unbox a => NonEmptyUVector a -> [a]
elemsU = VU.toList . _neUvec

-- | The elements of a non-empty vector as a non-empty list, read from its
-- representation.
--
elems1 :: NonEmptyVector a -> NonEmpty a
elems1 (NonEmptyVector v) = V.unsafeHead v :| V.toList (V.unsafeTail v)

-- | As 'elems1', but for unboxed representations.
--
elems1U :: VU.Unbox a => NonEmptyUVector a -> NonEmpty a
elems1U (NonEmptyUVector v) = VU.unsafeHead v :| VU.toList (VU.unsafeTail v)

-- | Model of the 'Maybe'-valued constructors: 'Nothing' exactly when the
-- result would be empty.
--
whenNonEmpty :: [a] -> Maybe [a]
whenNonEmpty [] = Nothing
whenNonEmpty as = Just as

-- ---------------------------------------------------------------------- --
-- Generators

-- | A valid index into the vector.
--
genIndex :: NonEmptyVector a -> Gen Int
genIndex (NonEmptyVector v) = choose (0, V.length v - 1)

-- | As 'genIndex', but for unboxed representations.
--
genIndexU :: VU.Unbox a => NonEmptyUVector a -> Gen Int
genIndexU (NonEmptyUVector v) = choose (0, VU.length v - 1)

-- | A non-empty vector of valid indices into the vector.
--
genIndices :: NonEmptyVector a -> Gen (NonEmptyVector Int)
genIndices v = vec <$> listOf1 (genIndex v)

-- | As 'genIndices', but for unboxed representations.
--
genIndicesU :: VU.Unbox a => NonEmptyUVector a -> Gen (NonEmptyUVector Int)
genIndicesU v = vecU <$> listOf1 (genIndexU v)

-- | An index outside the bounds of the vector, on either side.
--
genOutOfBounds :: NonEmptyVector a -> Gen Int
genOutOfBounds (NonEmptyVector v) = oneof
    [ choose (-10, -1)
    , choose (n, n + 10)
    ]
  where
    n = V.length v

-- | As 'genOutOfBounds', but for unboxed representations.
--
genOutOfBoundsU :: VU.Unbox a => NonEmptyUVector a -> Gen Int
genOutOfBoundsU (NonEmptyUVector v) = oneof
    [ choose (-10, -1)
    , choose (n, n + 10)
    ]
  where
    n = VU.length v

-- | A valid @(offset, length)@ slice of the vector, possibly empty.
--
genSlice :: NonEmptyVector a -> Gen (Int, Int)
genSlice (NonEmptyVector v) = do
    i <- choose (0, n)
    m <- choose (0, n - i)
    pure (i, m)
  where
    n = V.length v

-- | As 'genSlice', but for unboxed representations.
genSliceU :: VU.Unbox a => NonEmptyUVector a -> Gen (Int, Int)
genSliceU (NonEmptyUVector v) = do
    i <- choose (0, n)
    m <- choose (0, n - i)
    pure (i, m)
  where
    n = VU.length v

-- | A list of updates at valid indices of the vector.
--
genUpdates :: Arbitrary b => NonEmptyVector a -> Gen [(Int, b)]
genUpdates v = listOf ((,) <$> genIndex v <*> arbitrary)

-- | As 'genUpdates', but for unboxed representations.
--
genUpdatesU :: (VU.Unbox a, Arbitrary b) => NonEmptyUVector a -> Gen [(Int, b)]
genUpdatesU v = listOf ((,) <$> genIndexU v <*> arbitrary)

-- ---------------------------------------------------------------------- --
-- Effects

-- | A writer-style action that records @w@ and yields @a@. Running monadic
-- operations in @([w], -)@ exposes the order and number of effects.
--
logged :: w -> a -> ([w], a)
logged w a = ([w], a)

-- | A terminating unfold step: counts the seed down to zero, emitting
-- @f seed@ at each step.
--
unfoldStep :: (Int -> a) -> Int -> Maybe (a, Int)
unfoldStep f b
    | b <= 0 = Nothing
    | otherwise = Just (f b, b - 1)

-- ---------------------------------------------------------------------- --
-- List models

-- | Apply a function at an index, leaving the list unchanged when the index
-- is out of bounds.
--
adjustAt :: Int -> (a -> a) -> [a] -> [a]
adjustAt i f as = [ if j == i then f a else a | (j, a) <- zip [0 ..] as ]

-- | Total list indexing.
--
lookupList :: Int -> [a] -> Maybe a
lookupList i as
    | i < 0 = Nothing
    | otherwise = listToMaybe (drop i as)

-- | Exchange the elements at two indices.
--
swapAt :: Int -> Int -> [a] -> [a]
swapAt i j as = [ pick k a | (k, a) <- zip [0 ..] as ]
  where
    pick k a
      | k == i = fromMaybe a (lookupList j as)
      | k == j = fromMaybe a (lookupList i as)
      | otherwise = a

-- | Monadic 'Data.List.unfoldr'.
--
unfoldrMList :: Monad m => (b -> m (Maybe (a, b))) -> b -> m [a]
unfoldrMList f = go
  where
    go b = f b >>= maybe (pure []) (\(a, b') -> (a :) <$> go b')

-- | Monadic 'Data.List.unfoldr' producing at most @n@ elements. The step
-- function is not run once @n@ elements have been produced.
--
unfoldrNMList :: Monad m => Int -> (b -> m (Maybe (a, b))) -> b -> m [a]
unfoldrNMList n0 f = go n0
  where
    go n b
      | n <= 0 = pure []
      | otherwise = f b >>= maybe (pure []) (\(a, b') -> (a :) <$> go (n - 1) b')

-- ---------------------------------------------------------------------- --
-- Exceptions

-- | The value throws when fully evaluated.
--
throws :: NFData a => a -> Property
throws = throwsIO . evaluate . force

-- | The action throws when run.
--
throwsIO :: IO a -> Property
throwsIO act = ioProperty $ do
    r <- try act
    pure $ case r of
      Left (_ :: SomeException) -> property True
      Right _ -> counterexample "expected an exception" (property False)
