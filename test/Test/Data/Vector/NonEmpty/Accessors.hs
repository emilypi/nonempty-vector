-- |
-- Module      : Test.Data.Vector.NonEmpty.Accessors
-- Copyright   : (c) 2019-2026 Emily Pillmore
-- License     : BSD-style
--
-- Maintainer  : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability   : experimental
-- Portability : non-portable
--
-- Properties for length information, indexing, and slicing.
--
module Test.Data.Vector.NonEmpty.Accessors
( tests
) where


import Data.Functor.Identity (Identity(..))
import qualified Data.List as List
import qualified Data.Vector as V
import Data.Vector.NonEmpty (NonEmptyVector)
import qualified Data.Vector.NonEmpty as NEV

import Test.Data.Vector.NonEmpty.Util
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck


tests :: TestTree
tests = testGroup "Accessors"
    [ testGroup "Length information"
      [ testProperty "prop_length" prop_length
      ]
    , testGroup "Indexing"
      [ testProperty "prop_head" prop_head
      , testProperty "prop_last" prop_last
      , testProperty "prop_index" prop_index
      , testProperty "prop_index_out_of_bounds" prop_index_out_of_bounds
      , testProperty "prop_index_maybe" prop_index_maybe
      , testProperty "prop_unsafeIndex" prop_unsafeIndex
      ]
    , testGroup "Monadic indexing"
      [ testProperty "prop_headM" prop_headM
      , testProperty "prop_lastM" prop_lastM
      , testProperty "prop_indexM" prop_indexM
      , testProperty "prop_indexM_out_of_bounds" prop_indexM_out_of_bounds
      , testProperty "prop_unsafeIndexM" prop_unsafeIndexM
      ]
    , testGroup "Slicing"
      [ testProperty "prop_tail" prop_tail
      , testProperty "prop_init" prop_init
      , testProperty "prop_uncons" prop_uncons
      , testProperty "prop_unsnoc" prop_unsnoc
      , testProperty "prop_slice" prop_slice
      , testProperty "prop_slice_out_of_bounds" prop_slice_out_of_bounds
      , testProperty "prop_take" prop_take
      , testProperty "prop_drop" prop_drop
      , testProperty "prop_splitAt" prop_splitAt
      , testProperty "prop_unsafeSlice" prop_unsafeSlice
      , testProperty "prop_unsafeTake" prop_unsafeTake
      , testProperty "prop_unsafeDrop" prop_unsafeDrop
      ]
    ]

-- ---------------------------------------------------------------------- --
-- Length information

prop_length :: NonEmptyVector Int -> Property
prop_length v = NEV.length v === List.length (elems v)

-- ---------------------------------------------------------------------- --
-- Indexing

prop_head :: NonEmptyVector Int -> Property
prop_head v = [NEV.head v] === List.take 1 (elems v)

prop_last :: NonEmptyVector Int -> Property
prop_last v = [NEV.last v] === List.take 1 (List.reverse (elems v))

prop_index :: NonEmptyVector Int -> Property
prop_index v = forAll (genIndex v) $ \i ->
    Just (v NEV.! i) === lookupList i (elems v)

prop_index_out_of_bounds :: NonEmptyVector Int -> Property
prop_index_out_of_bounds v = forAll (genOutOfBounds v) $ \i ->
    throws (v NEV.! i)

prop_index_maybe :: NonEmptyVector Int -> Int -> Property
prop_index_maybe v i = v NEV.!? i === lookupList i (elems v)

prop_unsafeIndex :: NonEmptyVector Int -> Property
prop_unsafeIndex v = forAll (genIndex v) $ \i ->
    Just (NEV.unsafeIndex v i) === lookupList i (elems v)

-- ---------------------------------------------------------------------- --
-- Monadic indexing

prop_headM :: NonEmptyVector Int -> Property
prop_headM v = [runIdentity (NEV.headM v)] === List.take 1 (elems v)

prop_lastM :: NonEmptyVector Int -> Property
prop_lastM v =
    [runIdentity (NEV.lastM v)] === List.take 1 (List.reverse (elems v))

prop_indexM :: NonEmptyVector Int -> Property
prop_indexM v = forAll (genIndex v) $ \i ->
    Just (runIdentity (NEV.indexM v i)) === lookupList i (elems v)

prop_indexM_out_of_bounds :: NonEmptyVector Int -> Property
prop_indexM_out_of_bounds v = forAll (genOutOfBounds v) $ \i ->
    throws (runIdentity (NEV.indexM v i))

prop_unsafeIndexM :: NonEmptyVector Int -> Property
prop_unsafeIndexM v = forAll (genIndex v) $ \i ->
    Just (runIdentity (NEV.unsafeIndexM v i)) === lookupList i (elems v)

-- ---------------------------------------------------------------------- --
-- Slicing

prop_tail :: NonEmptyVector Int -> Property
prop_tail v = V.toList (NEV.tail v) === List.drop 1 (elems v)

prop_init :: NonEmptyVector Int -> Property
prop_init v =
    V.toList (NEV.init v) === List.take (List.length as - 1) as
  where
    as = elems v

prop_uncons :: NonEmptyVector Int -> Property
prop_uncons v = ([a], V.toList as) === List.splitAt 1 (elems v)
  where
    (a, as) = NEV.uncons v

prop_unsnoc :: NonEmptyVector Int -> Property
prop_unsnoc v =
    (V.toList as, [a]) === List.splitAt (List.length (elems v) - 1) (elems v)
  where
    (as, a) = NEV.unsnoc v

prop_slice :: NonEmptyVector Int -> Property
prop_slice v = forAll (genSlice v) $ \(i, m) ->
    V.toList (NEV.slice i m v) === List.take m (List.drop i (elems v))

prop_slice_out_of_bounds :: NonEmptyVector Int -> Property
prop_slice_out_of_bounds v = forAll genBadSlice $ \(i, m) ->
    throws (V.toList (NEV.slice i m v))
  where
    n = NEV.length v

    genBadSlice = oneof
      [ (,) <$> choose (-10, -1) <*> choose (0, n)
      , (,) <$> choose (0, n) <*> choose (-10, -1)
      , do
          i <- choose (0, n)
          m <- choose (n - i + 1, n - i + 10)
          pure (i, m)
      ]

prop_take :: Int -> NonEmptyVector Int -> Property
prop_take n v = V.toList (NEV.take n v) === List.take n (elems v)

prop_drop :: Int -> NonEmptyVector Int -> Property
prop_drop n v = V.toList (NEV.drop n v) === List.drop n (elems v)

prop_splitAt :: Int -> NonEmptyVector Int -> Property
prop_splitAt n v =
    (V.toList as, V.toList bs) === List.splitAt n (elems v)
  where
    (as, bs) = NEV.splitAt n v

prop_unsafeSlice :: NonEmptyVector Int -> Property
prop_unsafeSlice v = forAll (genSlice v) $ \(i, m) ->
    V.toList (NEV.unsafeSlice i m v) === List.take m (List.drop i (elems v))

prop_unsafeTake :: NonEmptyVector Int -> Property
prop_unsafeTake v = forAll (choose (0, NEV.length v)) $ \n ->
    V.toList (NEV.unsafeTake n v) === List.take n (elems v)

prop_unsafeDrop :: NonEmptyVector Int -> Property
prop_unsafeDrop v = forAll (choose (0, NEV.length v)) $ \n ->
    V.toList (NEV.unsafeDrop n v) === List.drop n (elems v)
