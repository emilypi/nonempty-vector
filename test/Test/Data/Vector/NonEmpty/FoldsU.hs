-- |
-- Module      : Test.Data.Vector.NonEmpty.FoldsU
-- Copyright   : (c) 2019-2026 Emily Pillmore, Koz Ross
-- License     : BSD-style
--
-- Maintainer  : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability   : experimental
-- Portability : non-portable
--
-- Properties for folds, specialized folds, monadic folds, and sequencing.
--
-- Fold functions are arbitrary, hence neither associative nor commutative,
-- so the models pin down fold direction and argument order. Monadic folds
-- run in a writer-style @([w], -)@ monad to expose effect order.
--
module Test.Data.Vector.NonEmpty.FoldsU
( tests
) where


import qualified Control.Monad as Monad
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty(..))
import Data.Ord (comparing)
import Data.Vector.Unboxed.NonEmpty (NonEmptyUVector)
import qualified Data.Vector.Unboxed.NonEmpty as NEV

import Test.Data.Vector.NonEmpty.Util
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck


tests :: TestTree
tests = testGroup "Folds (unboxed)"
    [ testGroup "Folding"
      [ testProperty "prop_foldl" prop_foldl
      , testProperty "prop_foldl1" prop_foldl1
      , testProperty "prop_foldl'" prop_foldl'
      , testProperty "prop_foldl1'" prop_foldl1'
      , testProperty "prop_foldr" prop_foldr
      , testProperty "prop_foldr1" prop_foldr1
      , testProperty "prop_foldr'" prop_foldr'
      , testProperty "prop_foldr1'" prop_foldr1'
      , testProperty "prop_ifoldl" prop_ifoldl
      , testProperty "prop_ifoldl'" prop_ifoldl'
      , testProperty "prop_ifoldr" prop_ifoldr
      , testProperty "prop_ifoldr'" prop_ifoldr'
      ]
    , testGroup "Specialized folds"
      [ testProperty "prop_all" prop_all
      , testProperty "prop_any" prop_any
      , testProperty "prop_and" prop_and
      , testProperty "prop_or" prop_or
      , testProperty "prop_sum" prop_sum
      , testProperty "prop_product" prop_product
      , testProperty "prop_maximum" prop_maximum
      , testProperty "prop_maximumBy" prop_maximumBy
      , testProperty "prop_minimum" prop_minimum
      , testProperty "prop_minimumBy" prop_minimumBy
      , testProperty "prop_maxIndex" prop_maxIndex
      , testProperty "prop_maxIndexBy" prop_maxIndexBy
      , testProperty "prop_minIndex" prop_minIndex
      , testProperty "prop_minIndexBy" prop_minIndexBy
      ]
    , testGroup "Monadic folds"
      [ testProperty "prop_foldM" prop_foldM
      , testProperty "prop_foldM'" prop_foldM'
      , testProperty "prop_fold1M" prop_fold1M
      , testProperty "prop_fold1M'" prop_fold1M'
      , testProperty "prop_foldM_" prop_foldM_
      , testProperty "prop_foldM'_" prop_foldM'_
      , testProperty "prop_fold1M_" prop_fold1M_
      , testProperty "prop_fold1M'_" prop_fold1M'_
      , testProperty "prop_ifoldM" prop_ifoldM
      , testProperty "prop_ifoldM'" prop_ifoldM'
      , testProperty "prop_ifoldM_" prop_ifoldM_
      , testProperty "prop_ifoldM'_" prop_ifoldM'_
      ]
    ]

type V = NonEmptyUVector Int

-- ---------------------------------------------------------------------- --
-- Models

indexedList :: V -> [(Int, Int)]
indexedList = List.zip [0 ..] . elemsU

-- | Ties resolve to the first occurrence, matching @vector@.
--
firstBy :: (a -> a -> Bool) -> [(Int, a)] -> (Int, a)
firstBy better = List.foldl1 (\x y -> if better (snd y) (snd x) then y else x)

-- | A comparison with many ties, so that tie-breaking is exercised.
--
coarse :: Int -> Int -> Ordering
coarse = comparing (`mod` 3)

-- | A writer-style step that logs the accumulator and element it consumes,
-- exposing how the accumulator is threaded even when the result is
-- discarded.
--
step :: (Int -> Int -> Int) -> Int -> Int -> ([(Int, Int)], Int)
step f acc a = logged (acc, a) (f acc a)

-- | A writer-style indexed step that logs the accumulator and index it
-- consumes.
--
istep
    :: (Int -> Int -> Int -> Int)
    -> Int
    -> Int
    -> Int
    -> ([(Int, Int)], Int)
istep f acc i a = logged (acc, i) (f acc i a)

-- ---------------------------------------------------------------------- --
-- Folding

prop_foldl :: Fun (Int, Int) Int -> Int -> V -> Property
prop_foldl (Fn2 f) z v = NEV.foldl f z v === List.foldl f z (elemsU v)

prop_foldl1 :: Fun (Int, Int) Int -> V -> Property
prop_foldl1 (Fn2 f) v = NEV.foldl1 f v === foldl1 f (elems1U v)

prop_foldl' :: Fun (Int, Int) Int -> Int -> V -> Property
prop_foldl' (Fn2 f) z v = NEV.foldl' f z v === List.foldl f z (elemsU v)

prop_foldl1' :: Fun (Int, Int) Int -> V -> Property
prop_foldl1' (Fn2 f) v = NEV.foldl1' f v === foldl1 f (elems1U v)

prop_foldr :: Fun (Int, Int) Int -> Int -> V -> Property
prop_foldr (Fn2 f) z v = NEV.foldr f z v === List.foldr f z (elemsU v)

prop_foldr1 :: Fun (Int, Int) Int -> V -> Property
prop_foldr1 (Fn2 f) v = NEV.foldr1 f v === foldr1 f (elems1U v)

prop_foldr' :: Fun (Int, Int) Int -> Int -> V -> Property
prop_foldr' (Fn2 f) z v = NEV.foldr' f z v === List.foldr f z (elemsU v)

prop_foldr1' :: Fun (Int, Int) Int -> V -> Property
prop_foldr1' (Fn2 f) v = NEV.foldr1' f v === foldr1 f (elems1U v)

prop_ifoldl :: Fun (Int, Int, Int) Int -> Int -> V -> Property
prop_ifoldl (Fn3 f) z v =
    NEV.ifoldl f z v
      === List.foldl (\acc (i, a) -> f acc i a) z (indexedList v)

prop_ifoldl' :: Fun (Int, Int, Int) Int -> Int -> V -> Property
prop_ifoldl' (Fn3 f) z v =
    NEV.ifoldl' f z v
      === List.foldl (\acc (i, a) -> f acc i a) z (indexedList v)

prop_ifoldr :: Fun (Int, Int, Int) Int -> Int -> V -> Property
prop_ifoldr (Fn3 f) z v =
    NEV.ifoldr f z v
      === List.foldr (\(i, a) acc -> f i a acc) z (indexedList v)

prop_ifoldr' :: Fun (Int, Int, Int) Int -> Int -> V -> Property
prop_ifoldr' (Fn3 f) z v =
    NEV.ifoldr' f z v
      === List.foldr (\(i, a) acc -> f i a acc) z (indexedList v)

-- ---------------------------------------------------------------------- --
-- Specialized folds

prop_all :: Fun Int Bool -> V -> Property
prop_all (Fn p) v = NEV.all p v === List.all p (elemsU v)

prop_any :: Fun Int Bool -> V -> Property
prop_any (Fn p) v = NEV.any p v === List.any p (elemsU v)

prop_and :: NonEmptyUVector Bool -> Property
prop_and v = NEV.and v === List.and (elemsU v)

prop_or :: NonEmptyUVector Bool -> Property
prop_or v = NEV.or v === List.or (elemsU v)

prop_sum :: V -> Property
prop_sum v = NEV.sum v === List.sum (elemsU v)

prop_product :: V -> Property
prop_product v = NEV.product v === List.product (elemsU v)

prop_maximum :: V -> Property
prop_maximum v = NEV.maximum v === maximum (elems1U v)

prop_maximumBy :: V -> Property
prop_maximumBy v =
    NEV.maximumBy coarse v
      === snd (firstBy (\a b -> coarse a b == GT) (indexedList v))

prop_minimum :: V -> Property
prop_minimum v = NEV.minimum v === minimum (elems1U v)

prop_minimumBy :: V -> Property
prop_minimumBy v =
    NEV.minimumBy coarse v
      === snd (firstBy (\a b -> coarse a b == LT) (indexedList v))

prop_maxIndex :: V -> Property
prop_maxIndex v =
    NEV.maxIndex w === fst (firstBy (>) (indexedList w))
  where
    w = vecU (List.map (`mod` 4) (elemsU v))

prop_maxIndexBy :: V -> Property
prop_maxIndexBy v =
    NEV.maxIndexBy coarse v
      === fst (firstBy (\a b -> coarse a b == GT) (indexedList v))

prop_minIndex :: V -> Property
prop_minIndex v =
    NEV.minIndex w === fst (firstBy (<) (indexedList w))
  where
    w = vecU (List.map (`mod` 4) (elemsU v))

prop_minIndexBy :: V -> Property
prop_minIndexBy v =
    NEV.minIndexBy coarse v
      === fst (firstBy (\a b -> coarse a b == LT) (indexedList v))

-- ---------------------------------------------------------------------- --
-- Monadic folds

prop_foldM :: Fun (Int, Int) Int -> Int -> V -> Property
prop_foldM (Fn2 f) z v =
    NEV.foldM (step f) z v === Monad.foldM (step f) z (elemsU v)

prop_foldM' :: Fun (Int, Int) Int -> Int -> V -> Property
prop_foldM' (Fn2 f) z v =
    NEV.foldM' (step f) z v === Monad.foldM (step f) z (elemsU v)

prop_fold1M :: Fun (Int, Int) Int -> V -> Property
prop_fold1M (Fn2 f) v =
    NEV.fold1M (step f) v === Monad.foldM (step f) a as
  where
    a :| as = elems1U v

prop_fold1M' :: Fun (Int, Int) Int -> V -> Property
prop_fold1M' (Fn2 f) v =
    NEV.fold1M' (step f) v === Monad.foldM (step f) a as
  where
    a :| as = elems1U v

prop_foldM_ :: Fun (Int, Int) Int -> Int -> V -> Property
prop_foldM_ (Fn2 f) z v =
    NEV.foldM_ (step f) z v === Monad.foldM_ (step f) z (elemsU v)

prop_foldM'_ :: Fun (Int, Int) Int -> Int -> V -> Property
prop_foldM'_ (Fn2 f) z v =
    NEV.foldM'_ (step f) z v === Monad.foldM_ (step f) z (elemsU v)

prop_fold1M_ :: Fun (Int, Int) Int -> V -> Property
prop_fold1M_ (Fn2 f) v =
    NEV.fold1M_ (step f) v === Monad.foldM_ (step f) a as
  where
    a :| as = elems1U v

prop_fold1M'_ :: Fun (Int, Int) Int -> V -> Property
prop_fold1M'_ (Fn2 f) v =
    NEV.fold1M'_ (step f) v === Monad.foldM_ (step f) a as
  where
    a :| as = elems1U v

prop_ifoldM :: Fun (Int, Int, Int) Int -> Int -> V -> Property
prop_ifoldM (Fn3 f) z v =
    NEV.ifoldM (istep f) z v
      === Monad.foldM (\acc (i, a) -> istep f acc i a) z (indexedList v)

prop_ifoldM' :: Fun (Int, Int, Int) Int -> Int -> V -> Property
prop_ifoldM' (Fn3 f) z v =
    NEV.ifoldM' (istep f) z v
      === Monad.foldM (\acc (i, a) -> istep f acc i a) z (indexedList v)

prop_ifoldM_ :: Fun (Int, Int, Int) Int -> Int -> V -> Property
prop_ifoldM_ (Fn3 f) z v =
    NEV.ifoldM_ (istep f) z v
      === Monad.foldM_ (\acc (i, a) -> istep f acc i a) z (indexedList v)

prop_ifoldM'_ :: Fun (Int, Int, Int) Int -> Int -> V -> Property
prop_ifoldM'_ (Fn3 f) z v =
    NEV.ifoldM'_ (istep f) z v
      === Monad.foldM_ (\acc (i, a) -> istep f acc i a) z (indexedList v)
