module Test.Data.Vector.NonEmpty.Grouping
( tests
) where


import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import Data.Vector.NonEmpty (NonEmptyVector)
import qualified Data.Vector.NonEmpty as NEV

import Test.Data.Vector.NonEmpty.Util
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck


tests :: TestTree
tests = testGroup "Grouping"
    [ testGroup "group"
      [ testProperty "prop_group" prop_group
      , testProperty "prop_group_singleton" prop_group_singleton
      , testProperty "prop_group_all_equal" prop_group_all_equal
      , testProperty "prop_group_distinct" prop_group_distinct
      , testProperty "prop_group_runs" prop_group_runs
      ]
    , testGroup "groupBy"
      [ testProperty "prop_groupBy" prop_groupBy
      , testProperty "prop_groupBy_singleton" prop_groupBy_singleton
      , testProperty "prop_groupBy_all_true" prop_groupBy_all_true
      , testProperty "prop_groupBy_all_false" prop_groupBy_all_false
      , testProperty "prop_groupBy_equivalence" prop_groupBy_equivalence
      , testProperty "prop_groupBy_nontransitive" prop_groupBy_nontransitive
      , testProperty "prop_groupBy_asymmetric" prop_groupBy_asymmetric
      ]
    ]

groupElems :: NonEmpty.NonEmpty (NonEmptyVector element) -> [[element]]
groupElems = List.map elems . NonEmpty.toList

prop_group :: NonEmptyVector Int -> Property
prop_group input =
    groupElems (NEV.group (vec values)) === List.group values
  where
    values = List.map (`mod` 4) (elems input)

prop_group_singleton :: Int -> Property
prop_group_singleton value =
    groupElems (NEV.group (vec [value])) === [[value]]

prop_group_all_equal :: Int -> Positive Int -> Property
prop_group_all_equal value (Positive count) =
    groupElems (NEV.group (vec values)) === [values]
  where
    values = List.replicate count value

prop_group_distinct :: NonEmptyVector Int -> Property
prop_group_distinct input =
    groupElems (NEV.group (vec values)) === List.map (: []) values
  where
    values = List.nub (elems input)

prop_group_runs :: Property
prop_group_runs = once $
    groupElems (NEV.group (vec "Mississippi"))
      === ["M", "i", "ss", "i", "ss", "i", "pp", "i"]

prop_groupBy :: Fun (Int, Int) Bool -> NonEmptyVector Int -> Property
prop_groupBy (Fn2 predicate) input =
    groupElems (NEV.groupBy predicate (vec values))
      === List.groupBy predicate values
  where
    values = List.map (`mod` 4) (elems input)

prop_groupBy_singleton :: Int -> Property
prop_groupBy_singleton value =
    groupElems (NEV.groupBy predicate (vec [value])) === [[value]]
  where
    predicate _ _ = error "groupBy evaluated its predicate on a singleton"

prop_groupBy_all_true :: NonEmptyVector Int -> Property
prop_groupBy_all_true input =
    groupElems (NEV.groupBy (\_ _ -> True) input) === [elems input]

prop_groupBy_all_false :: NonEmptyVector Int -> Property
prop_groupBy_all_false input =
    groupElems (NEV.groupBy (\_ _ -> False) input)
      === List.map (: []) (elems input)

prop_groupBy_equivalence :: NonEmptyVector Int -> Property
prop_groupBy_equivalence input =
    groupElems (NEV.groupBy sameParity input)
      === List.groupBy sameParity (elems input)
  where
    sameParity first second = even first == even second

prop_groupBy_nontransitive :: Property
prop_groupBy_nontransitive = once $
    groupElems (NEV.groupBy nearby (vec [1, 2, 3, 4] :: NonEmptyVector Int))
      === [[1, 2], [3, 4]]
  where
    nearby first second = abs (first - second) <= 1

prop_groupBy_asymmetric :: Property
prop_groupBy_asymmetric = once $
    groupElems (NEV.groupBy (<) (vec [1, 2, 1, 3, 0, 4] :: NonEmptyVector Int))
      === [[1, 2], [1, 3], [0, 4]]
