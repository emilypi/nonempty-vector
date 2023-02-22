{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- |
-- Module       : Main (tests)
-- Copyright 	: 2019-2023 (c) Emily Pillmore
-- License	: BSD
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: TypeFamilies
--
module Main
( main
) where


import Data.Maybe
import Data.Vector (Vector)
import Data.Vector.NonEmpty (NonEmptyVector)
import qualified Data.Vector.NonEmpty as NEV

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain $ testGroup "NonEmptyVector constructor"
    [ testProperty "prop_reverse" prop_reverse
    , testProperty "prop_from_to_list" prop_from_to_list
    , testProperty "prop_from_to_vec" prop_from_to_vec
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
