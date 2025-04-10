{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- |
-- Module   : Main (tests)
-- Copyright: 2019-2025 (c) Emily Pillmore
-- License  : BSD
--
-- Maintainer : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability  : Experimental
-- Portability: TypeFamilies
--
module Main
( main
) where


import Data.Maybe
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Vector.NonEmpty (NonEmptyVector)
import qualified Data.Vector.NonEmpty as NEV

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain $ testGroup "NonEmptyVector tests"
    [ testGroup "Basic operations" 
      [ testProperty "prop_reverse" prop_reverse
      , testProperty "prop_from_to_list" prop_from_to_list
      , testProperty "prop_from_to_vec" prop_from_to_vec
      ]
    , testGroup "Intersperse tests" 
      [ testProperty "prop_intersperse_length" prop_intersperse_length
      , testProperty "prop_intersperse_first_last" prop_intersperse_first_last
      , testProperty "prop_intersperse_separator" prop_intersperse_separator
      , testProperty "prop_intersperse_reconstruction" prop_intersperse_reconstruction
      , testProperty "prop_intersperse_singleton" prop_intersperse_singleton
      , testProperty "prop_explicit_examples" prop_explicit_examples
      ]
    ]

genList :: Gen [Int]
genList = listOf1 (choose (1, 100))

genNEV :: Gen (NonEmptyVector Int)
genNEV = fmap (fromJust . NEV.fromList) genList

genV :: Gen (Vector Int)
genV = NEV.toVector <$> genNEV

prop_reverse :: Property
prop_reverse =  forAll genNEV $ \t ->
  NEV.reverse (NEV.reverse t) == t

prop_from_to_list :: Property
prop_from_to_list =
  forAll genNEV $ \t ->
  forAll genList $ \u ->
    NEV.fromList (NEV.toList t) == Just t
      && fmap NEV.toList (NEV.fromList u) == Just u

prop_from_to_vec :: Property
prop_from_to_vec =
  forAll genNEV $ \t ->
  forAll genV $ \u ->
    NEV.fromVector (NEV.toVector t) == Just t
      && (NEV.toVector <$> NEV.fromVector u) == Just u

-- Helper function to reduce duplication in intersperse tests
withRandomVectorAndSep :: (NonEmptyVector Int -> Int -> Bool) -> Property
withRandomVectorAndSep f = forAll genNEV $ \v ->
  forAll (arbitrary :: Gen Int) $ \sep -> f v sep

-- Intersperse properties

-- For vectors with length > 1, interspersing should result in length 2*n - 1
prop_intersperse_length :: Property
prop_intersperse_length = withRandomVectorAndSep $ \v sep ->
  let result = NEV.intersperse sep v
      originalLen = NEV.length v
  in if originalLen > 1
     then NEV.length result == 2 * originalLen - 1
     else NEV.length result == originalLen

-- The first and last elements of the result should match the original vector
prop_intersperse_first_last :: Property
prop_intersperse_first_last = withRandomVectorAndSep $ \v sep ->
  let result = NEV.intersperse sep v
  in NEV.head result == NEV.head v && NEV.last result == NEV.last v

-- Every odd-indexed element should be the separator (for vectors with length > 1)
prop_intersperse_separator :: Property
prop_intersperse_separator = withRandomVectorAndSep $ \v sep ->
  let result = NEV.intersperse sep v
  in NEV.length v <= 1 || 
     all (\i -> result NEV.! (2*i+1) == sep) [0 .. NEV.length v - 2]

-- If we remove the separator, we should get back the original vector
prop_intersperse_reconstruction :: Property
prop_intersperse_reconstruction = withRandomVectorAndSep $ \v sep ->
  let result = NEV.intersperse sep v
      resultVector = NEV.toVector result
      reconstructed = V.ifilter (\i _ -> i `mod` 2 == 0) resultVector
  in V.toList reconstructed == NEV.toList v

-- For singleton vectors, interspersing should not change the vector
prop_intersperse_singleton :: Property
prop_intersperse_singleton = 
  forAll (arbitrary :: Gen Int) $ \x ->
    forAll (arbitrary :: Gen Int) $ \sep ->
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