-- |
-- Module      : Test.Data.Vector.NonEmpty.InstancesU
-- Copyright   : (c) 2019-2026 Emily Pillmore, Koz Ross
-- License     : BSD-style
--
-- Maintainer  : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability   : experimental
-- Portability : non-portable
--
-- Laws and list models for the type class instances of t'NonEmptyUVector'.
--
module Test.Data.Vector.NonEmpty.InstancesU
( tests
) where


import Control.DeepSeq (rnf)
import Control.Exception (evaluate)

import Data.Data (gmapM, gmapQ, gmapT, showConstr, toConstr)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup (sconcat, stimes)
import Data.Vector.NonEmptyU (NonEmptyUVector)
import qualified Data.Vector.NonEmptyU as NEV

import Test.Data.Vector.NonEmpty.Util
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

import Text.Read (readMaybe)


tests :: TestTree
tests = testGroup "Instances (unboxed)"
    [ testGroup "Eq"
      [ testProperty "prop_eq" prop_eq
      ]
    , testGroup "Ord"
      [ testProperty "prop_compare" prop_compare
      ]
    , testGroup "Show"
      [ testProperty "prop_show" prop_show
      , testProperty "prop_showsPrec" prop_showsPrec
      ]
    , testGroup "Read"
      [ testProperty "prop_read_show" prop_read_show
      , testProperty "prop_read_list" prop_read_list
      ]
    , testGroup "Semigroup"
      [ testProperty "prop_append" prop_append
      , testProperty "prop_semigroup_associativity" prop_semigroup_associativity
      , testProperty "prop_sconcat" prop_sconcat
      , testProperty "prop_stimes" prop_stimes
      , testProperty "prop_stimes_non_positive" prop_stimes_non_positive
      ]
    , testGroup "NFData"
      [ testProperty "prop_rnf" prop_rnf
      , testProperty "prop_rnf_forces_elements" prop_rnf_forces_elements
      ]
    , testGroup "Data"
      [ testProperty "prop_data" prop_data
      ]
    ]

type V = NonEmptyUVector Int

-- | Either the same vector or an unrelated one, so that equal pairs are
-- generated often.
--
genNear :: V -> Gen V
genNear v = oneof [pure v, arbitrary]

-- ---------------------------------------------------------------------- --
-- Eq and Ord

prop_eq :: V -> Property
prop_eq v = forAll (genNear v) $ \w -> (v == w) === (elemsU v == elemsU w)

prop_compare :: V -> Property
prop_compare v = forAll (genNear v) $ \w ->
    compare v w === compare (elemsU v) (elemsU w)

-- ---------------------------------------------------------------------- --
-- Show and Read

prop_show :: V -> Property
prop_show v = show v === show (elemsU v)

prop_showsPrec :: Int -> V -> String -> Property
prop_showsPrec d v s = showsPrec d v s === show (elemsU v) ++ s

prop_read_show :: V -> Property
prop_read_show v = read (show v) === v

-- The empty list must not parse.
prop_read_list :: [Int] -> Property
prop_read_list as =
    fmap elemsU (readMaybe (show as) :: Maybe V) === whenNonEmpty as

-- ---------------------------------------------------------------------- --
-- Semigroup

prop_append :: V -> V -> Property
prop_append v w = elemsU (v <> w) === elemsU v ++ elemsU w

prop_semigroup_associativity :: V -> V -> V -> Property
prop_semigroup_associativity u v w = ((u <> v) <> w) === (u <> (v <> w))

prop_sconcat :: V -> [V] -> Property
prop_sconcat v vs = elemsU (sconcat (v :| vs)) === List.concatMap elemsU (v : vs)

prop_stimes :: V -> Property
prop_stimes v = forAll (choose (1, 5)) $ \n ->
    elemsU (stimes (n :: Int) v) === List.concat (List.replicate n (elemsU v))

-- A non-positive multiplier would produce an empty vector, so it must throw.
prop_stimes_non_positive :: V -> Property
prop_stimes_non_positive v = forAll (choose (-5, 0)) $ \n ->
    throws (stimes (n :: Int) v)

-- ---------------------------------------------------------------------- --
-- NFData

prop_rnf :: V -> Property
prop_rnf v = rnf v === ()

prop_rnf_forces_elements :: Property
prop_rnf_forces_elements = once $
    throwsIO (evaluate (rnf (NEV.singleton (error "unforced element" :: Int))))

-- ---------------------------------------------------------------------- --
-- Data

-- The representation is a single constructor wrapping a single field.
prop_data :: V -> Property
prop_data v =
    gmapT id v === v
      .&&. gmapM Just v === Just v
      .&&. gmapQ (const ()) v === [()]
      .&&. showConstr (toConstr v) === "NonEmptyUVector"
