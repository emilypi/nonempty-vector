-- |
-- Module      : Test.Data.Vector.NonEmpty.ModificationU
-- Copyright   : (c) 2019-2026 Emily Pillmore, Koz Ross
-- License     : BSD-style
--
-- Maintainer  : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability   : experimental
-- Portability : non-portable
--
-- Properties for bulk updates, accumulations, permutations, and safe
-- destructive updates.
--
module Test.Data.Vector.NonEmpty.ModificationU
( tests
) where


import qualified Data.List as List
import Data.Maybe (mapMaybe)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Vector.NonEmptyU (NonEmptyUVector)
import qualified Data.Vector.NonEmptyU as NEV

import Test.Data.Vector.NonEmpty.Util
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck


tests :: TestTree
tests = testGroup "Modification (unboxed)"
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

prop_bulk_update :: NonEmptyUVector Int -> Property
prop_bulk_update v = forAll (genUpdatesU v) $ \us ->
    elemsU (v NEV.// us) === updateList us (elemsU v)

prop_bulk_update_out_of_bounds :: NonEmptyUVector Int -> Int -> Property
prop_bulk_update_out_of_bounds v a = forAll (genOutOfBoundsU v) $ \i ->
    throws (v NEV.// [(i, a)])

prop_update :: NonEmptyUVector Int -> Property
prop_update v = forAll (genUpdatesU v) $ \us ->
    elemsU (NEV.update v (V.fromList us)) === updateList us (elemsU v)

-- Surplus values beyond the index vector are ignored.
prop_update_ :: NonEmptyUVector Int -> [Int] -> Property
prop_update_ v surplus = forAll (genUpdatesU v) $ \us ->
    let (is, as) = List.unzip us
    in elemsU (NEV.update_ v (V.fromList is) (V.fromList (as ++ surplus)))
         === updateList us (elemsU v)

prop_unsafeUpd :: NonEmptyUVector Int -> Property
prop_unsafeUpd v = forAll (genUpdatesU v) $ \us ->
    elemsU (NEV.unsafeUpd v us) === updateList us (elemsU v)

prop_unsafeUpdate :: NonEmptyUVector Int -> Property
prop_unsafeUpdate v = forAll (genUpdatesU v) $ \us ->
    elemsU (NEV.unsafeUpdate v (V.fromList us)) === updateList us (elemsU v)

prop_unsafeUpdate_ :: NonEmptyUVector Int -> Property
prop_unsafeUpdate_ v = forAll (genUpdatesU v) $ \us ->
    let (is, as) = List.unzip us
    in elemsU (NEV.unsafeUpdate_ v (V.fromList is) (V.fromList as))
         === updateList us (elemsU v)

-- ---------------------------------------------------------------------- --
-- Accumulations

prop_accum :: Fun (Int, Int) Int -> NonEmptyUVector Int -> Property
prop_accum (Fn2 f) v = forAll (genUpdatesU v) $ \us ->
    elemsU (NEV.accum f v us) === accumList f us (elemsU v)

prop_accum_out_of_bounds :: NonEmptyUVector Int -> Int -> Property
prop_accum_out_of_bounds v b = forAll (genOutOfBoundsU v) $ \i ->
    throws (NEV.accum (+) v [(i, b)])

prop_accumulate :: Fun (Int, Int) Int -> NonEmptyUVector Int -> Property
prop_accumulate (Fn2 f) v = forAll (genUpdatesU v) $ \us ->
    elemsU (NEV.accumulate f v (V.fromList us)) === accumList f us (elemsU v)

prop_accumulate_ :: Fun (Int, Int) Int -> NonEmptyUVector Int -> Property
prop_accumulate_ (Fn2 f) v = forAll (genUpdatesU v) $ \us ->
    let (is, bs) = List.unzip us
    in elemsU (NEV.accumulate_ f v (V.fromList is) (V.fromList bs))
         === accumList f us (elemsU v)

prop_unsafeAccum :: Fun (Int, Int) Int -> NonEmptyUVector Int -> Property
prop_unsafeAccum (Fn2 f) v = forAll (genUpdatesU v) $ \us ->
    elemsU (NEV.unsafeAccum f v us) === accumList f us (elemsU v)

prop_unsafeAccumulate :: Fun (Int, Int) Int -> NonEmptyUVector Int -> Property
prop_unsafeAccumulate (Fn2 f) v = forAll (genUpdatesU v) $ \us ->
    elemsU (NEV.unsafeAccumulate f v (V.fromList us))
      === accumList f us (elemsU v)

prop_unsafeAccumulate_ :: Fun (Int, Int) Int -> NonEmptyUVector Int -> Property
prop_unsafeAccumulate_ (Fn2 f) v = forAll (genUpdatesU v) $ \us ->
    let (is, bs) = List.unzip us
    in elemsU (NEV.unsafeAccumulate_ f v (V.fromList is) (V.fromList bs))
         === accumList f us (elemsU v)

-- ---------------------------------------------------------------------- --
-- Permutations

prop_reverse :: NonEmptyUVector Int -> Property
prop_reverse v = elemsU (NEV.reverse v) === List.reverse (elemsU v)

prop_reverse_involutive :: NonEmptyUVector Int -> Property
prop_reverse_involutive v = NEV.reverse (NEV.reverse v) === v

prop_backpermute :: NonEmptyUVector Int -> Property
prop_backpermute v = forAll (genIndicesU v) $ \is ->
    elemsU (NEV.backpermute v is)
      === mapMaybe (`lookupList` elemsU v) (elemsU is)

prop_backpermute_out_of_bounds :: NonEmptyUVector Int -> Property
prop_backpermute_out_of_bounds v = forAll (genOutOfBoundsU v) $ \i ->
    throws (NEV.backpermute v (NEV.singleton i))

prop_unsafeBackpermute :: NonEmptyUVector Int -> Property
prop_unsafeBackpermute v = forAll (genIndicesU v) $ \is ->
    elemsU (NEV.unsafeBackpermute v is)
      === mapMaybe (`lookupList` elemsU v) (elemsU is)

-- ---------------------------------------------------------------------- --
-- Safe destructive updates

prop_modify :: NonEmptyUVector Int -> Int -> Property
prop_modify v a = forAll ((,) <$> genIndexU v <*> genIndexU v) $ \(i, j) ->
    elemsU (NEV.modify (\mv -> MV.write mv i a >> MV.swap mv i j) v)
      === swapAt i j (adjustAt i (const a) (elemsU v))

-- The input vector is copied, never mutated in place.
prop_modify_preserves_input :: NonEmptyList Int -> Int -> Property
prop_modify_preserves_input (NonEmpty as) a =
    (elemsU w, elemsU v) === (List.map (const a) as, as)
  where
    v = vecU as
    w = NEV.modify (`MV.set` a) v
