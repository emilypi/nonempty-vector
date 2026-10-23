-- |
-- Module      : Test.Data.Vector.NonEmpty.Modification
-- Copyright   : (c) 2019-2026 Emily Pillmore
-- License     : BSD-style
--
-- Maintainer  : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability   : experimental
-- Portability : non-portable
--
-- Properties for bulk updates, accumulations, permutations, and safe
-- destructive updates.
--
module Test.Data.Vector.NonEmpty.Modification
( tests
) where


import qualified Data.List as List
import Data.Maybe (mapMaybe)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.Vector.NonEmpty (NonEmptyVector)
import qualified Data.Vector.NonEmpty as NEV

import Test.Data.Vector.NonEmpty.Util
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck


tests :: TestTree
tests = testGroup "Modification"
    [ testGroup "Bulk updates"
      [ testProperty "prop_bulk_update" prop_bulk_update
      , testProperty "prop_bulk_update_out_of_bounds" prop_bulk_update_out_of_bounds
      , testProperty "prop_update" prop_update
      , testProperty "prop_update_" prop_update_
      , testProperty "prop_unsafeUpd" prop_unsafeUpd
      , testProperty "prop_unsafeUpdate" prop_unsafeUpdate
      , testProperty "prop_unsafeUpdate_" prop_unsafeUpdate_
      ]
    , testGroup "Accumulations"
      [ testProperty "prop_accum" prop_accum
      , testProperty "prop_accum_out_of_bounds" prop_accum_out_of_bounds
      , testProperty "prop_accumulate" prop_accumulate
      , testProperty "prop_accumulate_" prop_accumulate_
      , testProperty "prop_unsafeAccum" prop_unsafeAccum
      , testProperty "prop_unsafeAccumulate" prop_unsafeAccumulate
      , testProperty "prop_unsafeAccumulate_" prop_unsafeAccumulate_
      ]
    , testGroup "Permutations"
      [ testProperty "prop_reverse" prop_reverse
      , testProperty "prop_reverse_involutive" prop_reverse_involutive
      , testProperty "prop_backpermute" prop_backpermute
      , testProperty "prop_backpermute_out_of_bounds" prop_backpermute_out_of_bounds
      , testProperty "prop_unsafeBackpermute" prop_unsafeBackpermute
      ]
    , testGroup "Safe destructive updates"
      [ testProperty "prop_modify" prop_modify
      , testProperty "prop_modify_preserves_input" prop_modify_preserves_input
      ]
    ]

-- ---------------------------------------------------------------------- --
-- Models

-- | Later updates to the same index win.
--
updateList :: [(Int, a)] -> [a] -> [a]
updateList us as = List.foldl' (\acc (i, a) -> adjustAt i (const a) acc) as us

accumList :: (a -> b -> a) -> [(Int, b)] -> [a] -> [a]
accumList f us as = List.foldl' (\acc (i, b) -> adjustAt i (`f` b) acc) as us

-- ---------------------------------------------------------------------- --
-- Bulk updates

prop_bulk_update :: NonEmptyVector Int -> Property
prop_bulk_update v = forAll (genUpdates v) $ \us ->
    elems (v NEV.// us) === updateList us (elems v)

prop_bulk_update_out_of_bounds :: NonEmptyVector Int -> Int -> Property
prop_bulk_update_out_of_bounds v a = forAll (genOutOfBounds v) $ \i ->
    throws (v NEV.// [(i, a)])

prop_update :: NonEmptyVector Int -> Property
prop_update v = forAll (genUpdates v) $ \us ->
    elems (NEV.update v (V.fromList us)) === updateList us (elems v)

-- Surplus values beyond the index vector are ignored.
prop_update_ :: NonEmptyVector Int -> [Int] -> Property
prop_update_ v surplus = forAll (genUpdates v) $ \us ->
    let (is, as) = List.unzip us
    in elems (NEV.update_ v (V.fromList is) (V.fromList (as ++ surplus)))
         === updateList us (elems v)

prop_unsafeUpd :: NonEmptyVector Int -> Property
prop_unsafeUpd v = forAll (genUpdates v) $ \us ->
    elems (NEV.unsafeUpd v us) === updateList us (elems v)

prop_unsafeUpdate :: NonEmptyVector Int -> Property
prop_unsafeUpdate v = forAll (genUpdates v) $ \us ->
    elems (NEV.unsafeUpdate v (V.fromList us)) === updateList us (elems v)

prop_unsafeUpdate_ :: NonEmptyVector Int -> Property
prop_unsafeUpdate_ v = forAll (genUpdates v) $ \us ->
    let (is, as) = List.unzip us
    in elems (NEV.unsafeUpdate_ v (V.fromList is) (V.fromList as))
         === updateList us (elems v)

-- ---------------------------------------------------------------------- --
-- Accumulations

prop_accum :: Fun (Int, Int) Int -> NonEmptyVector Int -> Property
prop_accum (Fn2 f) v = forAll (genUpdates v) $ \us ->
    elems (NEV.accum f v us) === accumList f us (elems v)

prop_accum_out_of_bounds :: NonEmptyVector Int -> Int -> Property
prop_accum_out_of_bounds v b = forAll (genOutOfBounds v) $ \i ->
    throws (NEV.accum (+) v [(i, b)])

prop_accumulate :: Fun (Int, Int) Int -> NonEmptyVector Int -> Property
prop_accumulate (Fn2 f) v = forAll (genUpdates v) $ \us ->
    elems (NEV.accumulate f v (V.fromList us)) === accumList f us (elems v)

prop_accumulate_ :: Fun (Int, Int) Int -> NonEmptyVector Int -> Property
prop_accumulate_ (Fn2 f) v = forAll (genUpdates v) $ \us ->
    let (is, bs) = List.unzip us
    in elems (NEV.accumulate_ f v (V.fromList is) (V.fromList bs))
         === accumList f us (elems v)

prop_unsafeAccum :: Fun (Int, Int) Int -> NonEmptyVector Int -> Property
prop_unsafeAccum (Fn2 f) v = forAll (genUpdates v) $ \us ->
    elems (NEV.unsafeAccum f v us) === accumList f us (elems v)

prop_unsafeAccumulate :: Fun (Int, Int) Int -> NonEmptyVector Int -> Property
prop_unsafeAccumulate (Fn2 f) v = forAll (genUpdates v) $ \us ->
    elems (NEV.unsafeAccumulate f v (V.fromList us))
      === accumList f us (elems v)

prop_unsafeAccumulate_ :: Fun (Int, Int) Int -> NonEmptyVector Int -> Property
prop_unsafeAccumulate_ (Fn2 f) v = forAll (genUpdates v) $ \us ->
    let (is, bs) = List.unzip us
    in elems (NEV.unsafeAccumulate_ f v (V.fromList is) (V.fromList bs))
         === accumList f us (elems v)

-- ---------------------------------------------------------------------- --
-- Permutations

prop_reverse :: NonEmptyVector Int -> Property
prop_reverse v = elems (NEV.reverse v) === List.reverse (elems v)

prop_reverse_involutive :: NonEmptyVector Int -> Property
prop_reverse_involutive v = NEV.reverse (NEV.reverse v) === v

prop_backpermute :: NonEmptyVector Int -> Property
prop_backpermute v = forAll (genIndices v) $ \is ->
    elems (NEV.backpermute v is)
      === mapMaybe (`lookupList` elems v) (elems is)

prop_backpermute_out_of_bounds :: NonEmptyVector Int -> Property
prop_backpermute_out_of_bounds v = forAll (genOutOfBounds v) $ \i ->
    throws (NEV.backpermute v (NEV.singleton i))

prop_unsafeBackpermute :: NonEmptyVector Int -> Property
prop_unsafeBackpermute v = forAll (genIndices v) $ \is ->
    elems (NEV.unsafeBackpermute v is)
      === mapMaybe (`lookupList` elems v) (elems is)

-- ---------------------------------------------------------------------- --
-- Safe destructive updates

prop_modify :: NonEmptyVector Int -> Int -> Property
prop_modify v a = forAll ((,) <$> genIndex v <*> genIndex v) $ \(i, j) ->
    elems (NEV.modify (\mv -> MV.write mv i a >> MV.swap mv i j) v)
      === swapAt i j (adjustAt i (const a) (elems v))

-- The input vector is copied, never mutated in place.
prop_modify_preserves_input :: NonEmptyList Int -> Int -> Property
prop_modify_preserves_input (NonEmpty as) a =
    (elems w, elems v) === (List.map (const a) as, as)
  where
    v = vec as
    w = NEV.modify (`MV.set` a) v
