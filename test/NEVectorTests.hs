{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- |
-- Module       : Main (tests)
-- Copyright 	: 2019 (c) Emily Pillmore
-- License	: BSD
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: TypeFamilies
--
module Main
( main
) where


import Data.Functor
import Data.Maybe
import Data.Vector (Vector)
import Data.Vector.NonEmpty (NonEmptyVector)
import qualified Data.Vector.NonEmpty as NEV

import Hedgehog
import qualified Hedgehog.Range as Range

import qualified Hedgehog.Internal.Gen as Gen


main :: IO ()
main = void $ checkParallel $ Group "NonEmptyVector constructor"
    [ ("prop_reverse", prop_reverse)
    , ("prop_from_to_list", prop_from_to_list)
    , ("prop_from_to_vec", prop_from_to_vec)
    ]

genList :: Gen [Int]
genList = Gen.list (Range.linear 1 100) Gen.enumBounded

genNEV :: Gen (NonEmptyVector Int)
genNEV = fmap (fromJust . NEV.fromList) genList

genV :: Gen (Vector Int)
genV = NEV.toVector <$> genNEV

prop_reverse :: Property
prop_reverse = property $ do
  t <- forAll genNEV
  NEV.reverse (NEV.reverse t) === t

prop_from_to_list :: Property
prop_from_to_list = property $ do
    t <- forAll $ genNEV
    u <- forAll $ genList
    NEV.fromList (NEV.toList t) === Just t
    fmap NEV.toList (NEV.fromList u) === Just u

prop_from_to_vec :: Property
prop_from_to_vec = property $ do
    t <- forAll $ genNEV
    u <- forAll $ genV
    NEV.fromVector (NEV.toVector t) === Just t
    fmap NEV.toVector (NEV.fromVector u) === Just u
