-- |
-- Module      : Test.Data.Vector.NonEmpty.PredicatesU
-- Copyright   : (c) 2019-2026 Emily Pillmore, Koz Ross
-- License     : BSD-style
--
-- Maintainer  : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability   : experimental
-- Portability : non-portable
--
-- Properties for filtering, partitioning, and searching.
--
-- Elements are reduced modulo a small number where duplicates or hits
-- need to be likely.
--
module Test.Data.Vector.NonEmpty.PredicatesU
( tests
) where


import Data.Either (partitionEithers)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe
import qualified Data.Vector.Unboxed as V
import Data.Vector.NonEmptyU (NonEmptyUVector)
import qualified Data.Vector.NonEmptyU as NEV

import Test.Data.Vector.NonEmpty.Util
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck


tests :: TestTree
tests = testGroup "Predicates (unboxed)"
    [ testGroup "Filtering"
      [ testProperty "prop_uniq" prop_uniq
      , testProperty "prop_mapMaybe" prop_mapMaybe
      , testProperty "prop_imapMaybe" prop_imapMaybe
      , testProperty "prop_filter" prop_filter
      , testProperty "prop_ifilter" prop_ifilter
      , testProperty "prop_filterM" prop_filterM
      , testProperty "prop_ifilterM" prop_ifilterM
      , testProperty "prop_takeWhile" prop_takeWhile
      , testProperty "prop_dropWhile" prop_dropWhile
      ]
    , testGroup "Partitioning"
      [ testProperty "prop_partition" prop_partition
      , testProperty "prop_partitionWith" prop_partitionWith
      , testProperty "prop_unstablePartition" prop_unstablePartition
      , testProperty "prop_span" prop_span
      , testProperty "prop_break" prop_break
      ]
    , testGroup "Searching"
      [ testProperty "prop_elem" prop_elem
      , testProperty "prop_notElem" prop_notElem
      , testProperty "prop_find" prop_find
      , testProperty "prop_findIndex" prop_findIndex
      , testProperty "prop_findIndices" prop_findIndices
      , testProperty "prop_elemIndex" prop_elemIndex
      , testProperty "prop_elemIndices" prop_elemIndices
      ]
    ]

type V = NonEmptyUVector Int

-- | Reduce elements to a small range so that duplicates and matches are
-- common.
--
small :: V -> V
small = vecU . List.map (`mod` 4) . elemsU

-- ---------------------------------------------------------------------- --
-- Filtering

prop_uniq :: V -> Property
prop_uniq v =
    elemsU (NEV.uniq w) === List.map NonEmpty.head (NonEmpty.group (elemsU w))
  where
    w = small v

prop_mapMaybe :: Fun Int (Maybe Int) -> V -> Property
prop_mapMaybe (Fn f) v =
    V.toList (NEV.mapMaybe f v) === Maybe.mapMaybe f (elemsU v)

prop_imapMaybe :: Fun (Int, Int) (Maybe Int) -> V -> Property
prop_imapMaybe (Fn2 f) v =
    V.toList (NEV.imapMaybe f v)
      === Maybe.catMaybes (List.zipWith f [0 ..] (elemsU v))

prop_filter :: Fun Int Bool -> V -> Property
prop_filter (Fn p) v = V.toList (NEV.filter p v) === List.filter p (elemsU v)

prop_ifilter :: Fun (Int, Int) Bool -> V -> Property
prop_ifilter (Fn2 p) v =
    V.toList (NEV.ifilter p v)
      === [ a | (i, a) <- List.zip [0 ..] (elemsU v), p i a ]

prop_filterM :: Fun Int Bool -> V -> Property
prop_filterM (Fn p) v =
    fmap V.toList (NEV.filterM (\a -> logged a (p a)) v)
      === (elemsU v, List.filter p (elemsU v))

prop_ifilterM :: Fun (Int, Int) Bool -> V -> Property
prop_ifilterM (Fn2 p) v =
    fmap V.toList (NEV.ifilterM (\i a -> logged i (p i a)) v)
      === ( List.map fst ias
          , [ a | (i, a) <- ias, p i a ]
          )
  where
    ias = List.zip [0 ..] (elemsU v)

prop_takeWhile :: Fun Int Bool -> V -> Property
prop_takeWhile (Fn p) v =
    V.toList (NEV.takeWhile p v) === List.takeWhile p (elemsU v)

prop_dropWhile :: Fun Int Bool -> V -> Property
prop_dropWhile (Fn p) v =
    V.toList (NEV.dropWhile p v) === List.dropWhile p (elemsU v)

-- ---------------------------------------------------------------------- --
-- Partitioning

prop_partition :: Fun Int Bool -> V -> Property
prop_partition (Fn p) v =
    (V.toList as, V.toList bs) === List.partition p (elemsU v)
  where
    (as, bs) = NEV.partition p v

prop_partitionWith :: Fun Int (Either Int Int) -> V -> Property
prop_partitionWith (Fn f) v =
    (V.toList as, V.toList bs) === partitionEithers (List.map f (elemsU v))
  where
    (as, bs) = NEV.partitionWith f v

-- Only the contents of each side are specified, not their order.
prop_unstablePartition :: Fun Int Bool -> V -> Property
prop_unstablePartition (Fn p) v =
    (List.sort (V.toList as), List.sort (V.toList bs))
      === (List.sort ts, List.sort fs)
  where
    (as, bs) = NEV.unstablePartition p v
    (ts, fs) = List.partition p (elemsU v)

prop_span :: Fun Int Bool -> V -> Property
prop_span (Fn p) v =
    (V.toList as, V.toList bs) === List.span p (elemsU v)
  where
    (as, bs) = NEV.span p v

prop_break :: Fun Int Bool -> V -> Property
prop_break (Fn p) v =
    (V.toList as, V.toList bs) === List.break p (elemsU v)
  where
    (as, bs) = NEV.break p v

-- ---------------------------------------------------------------------- --
-- Searching

prop_elem :: Int -> V -> Property
prop_elem a v = NEV.elem (a `mod` 4) w === List.elem (a `mod` 4) (elemsU w)
  where
    w = small v

prop_notElem :: Int -> V -> Property
prop_notElem a v =
    NEV.notElem (a `mod` 4) w === List.notElem (a `mod` 4) (elemsU w)
  where
    w = small v

prop_find :: Fun Int Bool -> V -> Property
prop_find (Fn p) v = NEV.find p v === List.find p (elemsU v)

prop_findIndex :: Fun Int Bool -> V -> Property
prop_findIndex (Fn p) v = NEV.findIndex p v === List.findIndex p (elemsU v)

prop_findIndices :: Fun Int Bool -> V -> Property
prop_findIndices (Fn p) v =
    V.toList (NEV.findIndices p v) === List.findIndices p (elemsU v)

prop_elemIndex :: Int -> V -> Property
prop_elemIndex a v =
    NEV.elemIndex (a `mod` 4) w === List.elemIndex (a `mod` 4) (elemsU w)
  where
    w = small v

prop_elemIndices :: Int -> V -> Property
prop_elemIndices a v =
    V.toList (NEV.elemIndices (a `mod` 4) w)
      === List.elemIndices (a `mod` 4) (elemsU w)
  where
    w = small v
