-- |
-- Module      : Test.Data.Vector.NonEmpty.Conversion
-- Copyright   : (c) 2019-2026 Emily Pillmore
-- License     : BSD-style
--
-- Maintainer  : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability   : experimental
-- Portability : non-portable
--
-- Properties for conversions to and from non-empty lists, vectors, and
-- lists.
--
module Test.Data.Vector.NonEmpty.Conversion
( tests
) where


import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Vector as V
import Data.Vector.NonEmpty (NonEmptyVector)
import qualified Data.Vector.NonEmpty as NEV

import Test.Data.Vector.NonEmpty.Util
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck


tests :: TestTree
tests = testGroup "Conversion"
    [ testGroup "To/from non-empty lists"
      [ testProperty "prop_toNonEmpty" prop_toNonEmpty
      , testProperty "prop_fromNonEmpty" prop_fromNonEmpty
      , testProperty "prop_from_to_nonempty" prop_from_to_nonempty
      , testProperty "prop_fromNonEmptyN" prop_fromNonEmptyN
      , testProperty "prop_fromNonEmptyN1" prop_fromNonEmptyN1
      , testProperty "prop_unsafeFromList" prop_unsafeFromList
      ]
    , testGroup "To/from vector"
      [ testProperty "prop_toVector" prop_toVector
      , testProperty "prop_fromVector" prop_fromVector
      , testProperty "prop_unsafeFromVector" prop_unsafeFromVector
      , testProperty "prop_from_to_vec" prop_from_to_vec
      ]
    , testGroup "To/from list"
      [ testProperty "prop_toList" prop_toList
      , testProperty "prop_fromList" prop_fromList
      , testProperty "prop_fromListN" prop_fromListN
      , testProperty "prop_from_to_list" prop_from_to_list
      ]
    ]

-- ---------------------------------------------------------------------- --
-- To/from non-empty lists

prop_toNonEmpty :: NonEmptyVector Int -> Property
prop_toNonEmpty v = NEV.toNonEmpty v === elems1 v

prop_fromNonEmpty :: Int -> [Int] -> Property
prop_fromNonEmpty a as = elems (NEV.fromNonEmpty (a :| as)) === a : as

prop_from_to_nonempty :: NonEmptyVector Int -> Property
prop_from_to_nonempty v = NEV.fromNonEmpty (NEV.toNonEmpty v) === v

prop_fromNonEmptyN :: Int -> Int -> [Int] -> Property
prop_fromNonEmptyN n a as =
    fmap elems (NEV.fromNonEmptyN n (a :| as))
      === whenNonEmpty (List.take n (a : as))

prop_fromNonEmptyN1 :: Int -> Int -> [Int] -> Property
prop_fromNonEmptyN1 n a as =
    elems (NEV.fromNonEmptyN1 n (a :| as)) === List.take (max n 1) (a : as)

prop_unsafeFromList :: NonEmptyList Int -> Property
prop_unsafeFromList (NonEmpty as) = elems (NEV.unsafeFromList as) === as

-- ---------------------------------------------------------------------- --
-- To/from vector

prop_toVector :: NonEmptyVector Int -> Property
prop_toVector v = V.toList (NEV.toVector v) === elems v

prop_fromVector :: [Int] -> Property
prop_fromVector as =
    fmap elems (NEV.fromVector (V.fromList as)) === whenNonEmpty as

prop_unsafeFromVector :: NonEmptyList Int -> Property
prop_unsafeFromVector (NonEmpty as) =
    elems (NEV.unsafeFromVector (V.fromList as)) === as

prop_from_to_vec :: NonEmptyVector Int -> NonEmptyList Int -> Property
prop_from_to_vec t (NonEmpty as) =
    NEV.fromVector (NEV.toVector t) === Just t
      .&&. (NEV.toVector <$> NEV.fromVector u) === Just u
  where
    u = V.fromList as

-- ---------------------------------------------------------------------- --
-- To/from list

prop_toList :: NonEmptyVector Int -> Property
prop_toList v = NEV.toList v === elems v

prop_fromList :: [Int] -> Property
prop_fromList as = fmap elems (NEV.fromList as) === whenNonEmpty as

prop_fromListN :: Int -> [Int] -> Property
prop_fromListN n as =
    fmap elems (NEV.fromListN n as) === whenNonEmpty (List.take n as)

prop_from_to_list :: NonEmptyVector Int -> NonEmptyList Int -> Property
prop_from_to_list t (NonEmpty u) =
    NEV.fromList (NEV.toList t) === Just t
      .&&. fmap NEV.toList (NEV.fromList u) === Just u
