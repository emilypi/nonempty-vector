-- |
-- Module      : Test.Data.Vector.NonEmpty.ScansU
-- Copyright   : (c) 2019-2026 Emily Pillmore, Koz Ross
-- License     : BSD-style
--
-- Maintainer  : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability   : experimental
-- Portability : non-portable
--
-- Properties for prefix sums (scans) and transformations.
--
-- Scan functions are arbitrary, hence neither associative nor commutative,
-- so the models pin down scan direction and argument order.
--
module Test.Data.Vector.NonEmpty.ScansU
( tests
) where


import qualified Data.List as List
import qualified Data.Vector.Unboxed as V
import Data.Vector.NonEmpty.Unboxed (NonEmptyUVector)
import qualified Data.Vector.NonEmpty.Unboxed as NEV

import Test.Data.Vector.NonEmpty.Util
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck


tests :: TestTree
tests = testGroup "Scans (unboxed)"
    [ testGroup "Prefix sums"
      [ testProperty "prop_prescanl" prop_prescanl
      , testProperty "prop_prescanl'" prop_prescanl'
      , testProperty "prop_postscanl" prop_postscanl
      , testProperty "prop_postscanl'" prop_postscanl'
      , testProperty "prop_scanl" prop_scanl
      , testProperty "prop_scanl'" prop_scanl'
      , testProperty "prop_scanl1" prop_scanl1
      , testProperty "prop_scanl1'" prop_scanl1'
      , testProperty "prop_iscanl" prop_iscanl
      , testProperty "prop_iscanl'" prop_iscanl'
      , testProperty "prop_prescanr" prop_prescanr
      , testProperty "prop_prescanr'" prop_prescanr'
      , testProperty "prop_postscanr" prop_postscanr
      , testProperty "prop_postscanr'" prop_postscanr'
      , testProperty "prop_scanr" prop_scanr
      , testProperty "prop_scanr'" prop_scanr'
      , testProperty "prop_scanr1" prop_scanr1
      , testProperty "prop_scanr1'" prop_scanr1'
      , testProperty "prop_iscanr" prop_iscanr
      , testProperty "prop_iscanr'" prop_iscanr'
      ]
    , testGroup "Intersperse tests"
      [ testProperty "prop_intersperse" prop_intersperse
      , testProperty "prop_intersperse_length" prop_intersperse_length
      , testProperty "prop_intersperse_first_last" prop_intersperse_first_last
      , testProperty "prop_intersperse_separator" prop_intersperse_separator
      , testProperty "prop_intersperse_reconstruction" prop_intersperse_reconstruction
      , testProperty "prop_intersperse_singleton" prop_intersperse_singleton
      , testProperty "prop_explicit_examples" prop_explicit_examples
      ]
    ]

type V = NonEmptyUVector Int

-- ---------------------------------------------------------------------- --
-- Models

-- | Left scan over @(index, element)@ pairs.
--
iscanlList :: (Int -> a -> b -> a) -> a -> [b] -> [a]
iscanlList f z = List.scanl (\acc (i, b) -> f i acc b) z . List.zip [0 ..]

-- | Right scan over @(index, element)@ pairs.
--
iscanrList :: (Int -> a -> b -> b) -> b -> [a] -> [b]
iscanrList f z = List.scanr (\(i, a) acc -> f i a acc) z . List.zip [0 ..]

-- ---------------------------------------------------------------------- --
-- Left scans

prop_prescanl :: Fun (Int, Int) Int -> Int -> V -> Property
prop_prescanl (Fn2 f) z v =
    elemsU (NEV.prescanl f z v)
      === List.take (NEV.length v) (List.scanl f z (elemsU v))

prop_prescanl' :: Fun (Int, Int) Int -> Int -> V -> Property
prop_prescanl' (Fn2 f) z v =
    elemsU (NEV.prescanl' f z v)
      === List.take (NEV.length v) (List.scanl f z (elemsU v))

prop_postscanl :: Fun (Int, Int) Int -> Int -> V -> Property
prop_postscanl (Fn2 f) z v =
    elemsU (NEV.postscanl f z v) === List.drop 1 (List.scanl f z (elemsU v))

prop_postscanl' :: Fun (Int, Int) Int -> Int -> V -> Property
prop_postscanl' (Fn2 f) z v =
    elemsU (NEV.postscanl' f z v) === List.drop 1 (List.scanl f z (elemsU v))

prop_scanl :: Fun (Int, Int) Int -> Int -> V -> Property
prop_scanl (Fn2 f) z v = elemsU (NEV.scanl f z v) === List.scanl f z (elemsU v)

prop_scanl' :: Fun (Int, Int) Int -> Int -> V -> Property
prop_scanl' (Fn2 f) z v = elemsU (NEV.scanl' f z v) === List.scanl f z (elemsU v)

prop_scanl1 :: Fun (Int, Int) Int -> V -> Property
prop_scanl1 (Fn2 f) v = elemsU (NEV.scanl1 f v) === List.scanl1 f (elemsU v)

prop_scanl1' :: Fun (Int, Int) Int -> V -> Property
prop_scanl1' (Fn2 f) v = elemsU (NEV.scanl1' f v) === List.scanl1 f (elemsU v)

prop_iscanl :: Fun (Int, Int, Int) Int -> Int -> V -> Property
prop_iscanl (Fn3 f) z v =
    elemsU (NEV.iscanl f z v) === iscanlList f z (elemsU v)

prop_iscanl' :: Fun (Int, Int, Int) Int -> Int -> V -> Property
prop_iscanl' (Fn3 f) z v =
    elemsU (NEV.iscanl' f z v) === iscanlList f z (elemsU v)

-- ---------------------------------------------------------------------- --
-- Right scans

prop_prescanr :: Fun (Int, Int) Int -> Int -> V -> Property
prop_prescanr (Fn2 f) z v =
    elemsU (NEV.prescanr f z v) === List.drop 1 (List.scanr f z (elemsU v))

prop_prescanr' :: Fun (Int, Int) Int -> Int -> V -> Property
prop_prescanr' (Fn2 f) z v =
    elemsU (NEV.prescanr' f z v) === List.drop 1 (List.scanr f z (elemsU v))

prop_postscanr :: Fun (Int, Int) Int -> Int -> V -> Property
prop_postscanr (Fn2 f) z v =
    elemsU (NEV.postscanr f z v)
      === List.take (NEV.length v) (List.scanr f z (elemsU v))

prop_postscanr' :: Fun (Int, Int) Int -> Int -> V -> Property
prop_postscanr' (Fn2 f) z v =
    elemsU (NEV.postscanr' f z v)
      === List.take (NEV.length v) (List.scanr f z (elemsU v))

prop_scanr :: Fun (Int, Int) Int -> Int -> V -> Property
prop_scanr (Fn2 f) z v = elemsU (NEV.scanr f z v) === List.scanr f z (elemsU v)

prop_scanr' :: Fun (Int, Int) Int -> Int -> V -> Property
prop_scanr' (Fn2 f) z v = elemsU (NEV.scanr' f z v) === List.scanr f z (elemsU v)

prop_scanr1 :: Fun (Int, Int) Int -> V -> Property
prop_scanr1 (Fn2 f) v = elemsU (NEV.scanr1 f v) === List.scanr1 f (elemsU v)

prop_scanr1' :: Fun (Int, Int) Int -> V -> Property
prop_scanr1' (Fn2 f) v = elemsU (NEV.scanr1' f v) === List.scanr1 f (elemsU v)

prop_iscanr :: Fun (Int, Int, Int) Int -> Int -> V -> Property
prop_iscanr (Fn3 f) z v =
    elemsU (NEV.iscanr f z v) === iscanrList f z (elemsU v)

prop_iscanr' :: Fun (Int, Int, Int) Int -> Int -> V -> Property
prop_iscanr' (Fn3 f) z v =
    elemsU (NEV.iscanr' f z v) === iscanrList f z (elemsU v)

-- ---------------------------------------------------------------------- --
-- Intersperse properties

prop_intersperse :: Int -> V -> Property
prop_intersperse sep v =
    elemsU (NEV.intersperse sep v) === List.intersperse sep (elemsU v)

-- For vectors with length > 1, interspersing should result in length 2*n - 1
prop_intersperse_length :: Int -> V -> Bool
prop_intersperse_length sep v =
  let result = NEV.intersperse sep v
      originalLen = NEV.length v
  in if originalLen > 1
     then NEV.length result == 2 * originalLen - 1
     else NEV.length result == originalLen

-- The first and last elements of the result should match the original vector
prop_intersperse_first_last :: Int -> V -> Bool
prop_intersperse_first_last sep v =
  let result = NEV.intersperse sep v
  in NEV.head result == NEV.head v && NEV.last result == NEV.last v

-- Every odd-indexed element should be the separator (for vectors with length > 1)
prop_intersperse_separator :: Int -> V -> Bool
prop_intersperse_separator sep v =
  let result = NEV.intersperse sep v
  in NEV.length v <= 1 ||
     all (\i -> result NEV.! (2*i+1) == sep) [0 .. NEV.length v - 2]

-- If we remove the separator, we should get back the original vector
prop_intersperse_reconstruction :: Int -> V -> Bool
prop_intersperse_reconstruction sep v =
  let result = NEV.intersperse sep v
      resultVector = NEV.toVector result
      reconstructed = V.ifilter (\i _ -> i `mod` 2 == 0) resultVector
  in V.toList reconstructed == NEV.toList v

-- For singleton vectors, interspersing should not change the vector
prop_intersperse_singleton :: Int -> Int -> Bool
prop_intersperse_singleton x sep =
  let singleton = NEV.singleton x
      result = NEV.intersperse sep singleton
  in result == singleton

-- Explicit test cases for edge cases and examples combined into one property
prop_explicit_examples :: Property
prop_explicit_examples =
  let normalCase = NEV.intersperse (0 :: Int) (NEV.unsafeFromList [1,2,3 :: Int])
                   === NEV.unsafeFromList [1,0,2,0,3 :: Int]

      singletonCase = NEV.intersperse (0 :: Int) (NEV.singleton (5 :: Int))
                      === NEV.singleton (5 :: Int)
  in normalCase .&&. singletonCase
