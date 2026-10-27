-- |
-- Module      : Test.Data.Vector.NonEmpty.AccessorsU
-- Copyright   : (c) 2019-2026 Emily Pillmore, Koz Ross
-- License     : BSD-style
--
-- Maintainer  : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability   : experimental
-- Portability : non-portable
--
-- Properties for length information, indexing, and slicing.
--
module Test.Data.Vector.NonEmpty.AccessorsU
( tests
) where


import Data.Functor.Identity (Identity(..))
import qualified Data.List as List
import qualified Data.Vector.Unboxed as V
import Data.Vector.NonEmpty.Unboxed (NonEmptyUVector)
import qualified Data.Vector.NonEmpty.Unboxed as NEV

import Test.Data.Vector.NonEmpty.Util
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck


tests :: TestTree
tests = testGroup "Accessors (unboxed)"
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

prop_length :: NonEmptyUVector Int -> Property
prop_length v = NEV.length v === List.length (elemsU v)

-- ---------------------------------------------------------------------- --
-- Indexing

prop_head :: NonEmptyUVector Int -> Property
prop_head v = [NEV.head v] === List.take 1 (elemsU v)

prop_last :: NonEmptyUVector Int -> Property
prop_last v = [NEV.last v] === List.take 1 (List.reverse (elemsU v))

prop_index :: NonEmptyUVector Int -> Property
prop_index v = forAll (genIndexU v) $ \i ->
    Just (v NEV.! i) === lookupList i (elemsU v)

prop_index_out_of_bounds :: NonEmptyUVector Int -> Property
prop_index_out_of_bounds v = forAll (genOutOfBoundsU v) $ \i ->
    throws (v NEV.! i)

prop_index_maybe :: NonEmptyUVector Int -> Int -> Property
prop_index_maybe v i = v NEV.!? i === lookupList i (elemsU v)

prop_unsafeIndex :: NonEmptyUVector Int -> Property
prop_unsafeIndex v = forAll (genIndexU v) $ \i ->
    Just (NEV.unsafeIndex v i) === lookupList i (elemsU v)

-- ---------------------------------------------------------------------- --
-- Monadic indexing

prop_headM :: NonEmptyUVector Int -> Property
prop_headM v = [runIdentity (NEV.headM v)] === List.take 1 (elemsU v)

prop_lastM :: NonEmptyUVector Int -> Property
prop_lastM v =
    [runIdentity (NEV.lastM v)] === List.take 1 (List.reverse (elemsU v))

prop_indexM :: NonEmptyUVector Int -> Property
prop_indexM v = forAll (genIndexU v) $ \i ->
    Just (runIdentity (NEV.indexM v i)) === lookupList i (elemsU v)

prop_indexM_out_of_bounds :: NonEmptyUVector Int -> Property
prop_indexM_out_of_bounds v = forAll (genOutOfBoundsU v) $ \i ->
    throws (runIdentity (NEV.indexM v i))

prop_unsafeIndexM :: NonEmptyUVector Int -> Property
prop_unsafeIndexM v = forAll (genIndexU v) $ \i ->
    Just (runIdentity (NEV.unsafeIndexM v i)) === lookupList i (elemsU v)

-- ---------------------------------------------------------------------- --
-- Slicing

prop_tail :: NonEmptyUVector Int -> Property
prop_tail v = V.toList (NEV.tail v) === List.drop 1 (elemsU v)

prop_init :: NonEmptyUVector Int -> Property
prop_init v =
    V.toList (NEV.init v) === List.take (List.length as - 1) as
  where
    as = elemsU v

prop_uncons :: NonEmptyUVector Int -> Property
prop_uncons v = ([a], V.toList as) === List.splitAt 1 (elemsU v)
  where
    (a, as) = NEV.uncons v

prop_unsnoc :: NonEmptyUVector Int -> Property
prop_unsnoc v =
    (V.toList as, [a]) === List.splitAt (List.length (elemsU v) - 1) (elemsU v)
  where
    (as, a) = NEV.unsnoc v

prop_slice :: NonEmptyUVector Int -> Property
prop_slice v = forAll (genSliceU v) $ \(i, m) ->
    V.toList (NEV.slice i m v) === List.take m (List.drop i (elemsU v))

prop_slice_out_of_bounds :: NonEmptyUVector Int -> Property
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

prop_take :: Int -> NonEmptyUVector Int -> Property
prop_take n v = V.toList (NEV.take n v) === List.take n (elemsU v)

prop_drop :: Int -> NonEmptyUVector Int -> Property
prop_drop n v = V.toList (NEV.drop n v) === List.drop n (elemsU v)

prop_splitAt :: Int -> NonEmptyUVector Int -> Property
prop_splitAt n v =
    (V.toList as, V.toList bs) === List.splitAt n (elemsU v)
  where
    (as, bs) = NEV.splitAt n v

prop_unsafeSlice :: NonEmptyUVector Int -> Property
prop_unsafeSlice v = forAll (genSliceU v) $ \(i, m) ->
    V.toList (NEV.unsafeSlice i m v) === List.take m (List.drop i (elemsU v))

prop_unsafeTake :: NonEmptyUVector Int -> Property
prop_unsafeTake v = forAll (choose (0, NEV.length v)) $ \n ->
    V.toList (NEV.unsafeTake n v) === List.take n (elemsU v)

prop_unsafeDrop :: NonEmptyUVector Int -> Property
prop_unsafeDrop v = forAll (choose (0, NEV.length v)) $ \n ->
    V.toList (NEV.unsafeDrop n v) === List.drop n (elemsU v)
