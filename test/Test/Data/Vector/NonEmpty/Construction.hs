-- |
-- Module      : Test.Data.Vector.NonEmpty.Construction
-- Copyright   : (c) 2019-2026 Emily Pillmore
-- License     : BSD-style
--
-- Maintainer  : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability   : experimental
-- Portability : non-portable
--
-- Properties for initialization, unfolding, enumeration, and
-- concatenation.
--
-- The 'Maybe'-valued constructors must yield 'Nothing' exactly when the
-- result would be empty, and the @*1@ variants must clamp their size to at
-- least one.
--
module Test.Data.Vector.NonEmpty.Construction
( tests
) where


import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Vector as V
import qualified Data.Vector.NonEmpty as NEV
import Data.Vector.NonEmpty.Internal (NonEmptyVector(..))

import Test.Data.Vector.NonEmpty.Util
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck


tests :: TestTree
tests = testGroup "Construction"
    [ testGroup "Initialization"
      [ testProperty "prop_singleton" prop_singleton
      , testProperty "prop_replicate" prop_replicate
      , testProperty "prop_replicate1" prop_replicate1
      , testProperty "prop_generate" prop_generate
      , testProperty "prop_generate1" prop_generate1
      , testProperty "prop_iterateN" prop_iterateN
      , testProperty "prop_iterateN1" prop_iterateN1
      ]
    , testGroup "Monadic initialization"
      [ testProperty "prop_replicateM" prop_replicateM
      , testProperty "prop_replicate1M" prop_replicate1M
      , testProperty "prop_generateM" prop_generateM
      , testProperty "prop_generate1M" prop_generate1M
      , testProperty "prop_iterateNM" prop_iterateNM
      , testProperty "prop_iterateN1M" prop_iterateN1M
      , testProperty "prop_create" prop_create
      , testProperty "prop_unsafeCreate" prop_unsafeCreate
      , testProperty "prop_createT" prop_createT
      , testProperty "prop_unsafeCreateT" prop_unsafeCreateT
      ]
    , testGroup "Unfolding"
      [ testProperty "prop_unfoldr" prop_unfoldr
      , testProperty "prop_unfoldr1" prop_unfoldr1
      , testProperty "prop_unfoldrN" prop_unfoldrN
      , testProperty "prop_unfoldr1N" prop_unfoldr1N
      , testProperty "prop_unfoldrM" prop_unfoldrM
      , testProperty "prop_unfoldr1M" prop_unfoldr1M
      , testProperty "prop_unfoldrNM" prop_unfoldrNM
      , testProperty "prop_unfoldr1NM" prop_unfoldr1NM
      , testProperty "prop_constructN" prop_constructN
      , testProperty "prop_constructrN" prop_constructrN
      ]
    , testGroup "Enumeration"
      [ testProperty "prop_enumFromN" prop_enumFromN
      , testProperty "prop_enumFromN1" prop_enumFromN1
      , testProperty "prop_enumFromStepN" prop_enumFromStepN
      , testProperty "prop_enumFromStepN1" prop_enumFromStepN1
      , testProperty "prop_enumFromTo" prop_enumFromTo
      , testProperty "prop_enumFromThenTo" prop_enumFromThenTo
      ]
    , testGroup "Concatenation"
      [ testProperty "prop_cons" prop_cons
      , testProperty "prop_consV" prop_consV
      , testProperty "prop_snoc" prop_snoc
      , testProperty "prop_snocV" prop_snocV
      , testProperty "prop_append" prop_append
      , testProperty "prop_concat" prop_concat
      , testProperty "prop_concat1" prop_concat1
      ]
    , testGroup "Restricting memory usage"
      [ testProperty "prop_force" prop_force
      ]
    ]

-- ---------------------------------------------------------------------- --
-- Initialization

prop_singleton :: Int -> Property
prop_singleton a = elems (NEV.singleton a) === [a]

prop_replicate :: Int -> Int -> Property
prop_replicate n a =
    fmap elems (NEV.replicate n a) === whenNonEmpty (List.replicate n a)

prop_replicate1 :: Int -> Int -> Property
prop_replicate1 n a =
    elems (NEV.replicate1 n a) === List.replicate (max n 1) a

prop_generate :: Int -> Fun Int Int -> Property
prop_generate n (Fn f) =
    fmap elems (NEV.generate n f) === whenNonEmpty (fmap f [0 .. n - 1])

prop_generate1 :: Int -> Fun Int Int -> Property
prop_generate1 n (Fn f) =
    elems (NEV.generate1 n f) === fmap f [0 .. max n 1 - 1]

prop_iterateN :: Int -> Fun Int Int -> Int -> Property
prop_iterateN n (Fn f) a =
    fmap elems (NEV.iterateN n f a)
      === whenNonEmpty (List.take n (List.iterate f a))

prop_iterateN1 :: Int -> Fun Int Int -> Int -> Property
prop_iterateN1 n (Fn f) a =
    elems (NEV.iterateN1 n f a) === List.take (max n 1) (List.iterate f a)

-- ---------------------------------------------------------------------- --
-- Monadic initialization

prop_replicateM :: Int -> Int -> Property
prop_replicateM n a =
    fmap (fmap elems) (NEV.replicateM n (logged () a))
      === (List.replicate n (), whenNonEmpty (List.replicate n a))

prop_replicate1M :: Int -> Int -> Property
prop_replicate1M n a =
    fmap elems (NEV.replicate1M n (logged () a))
      === (List.replicate n' (), List.replicate n' a)
  where
    n' = max n 1

prop_generateM :: Int -> Fun Int Int -> Property
prop_generateM n (Fn f) =
    fmap (fmap elems) (NEV.generateM n (\i -> logged i (f i)))
      === (is, whenNonEmpty (fmap f is))
  where
    is = [0 .. n - 1]

prop_generate1M :: Int -> Fun Int Int -> Property
prop_generate1M n (Fn f) =
    fmap elems (NEV.generate1M n (\i -> logged i (f i)))
      === (is, fmap f is)
  where
    is = [0 .. max n 1 - 1]

-- The step function runs once per element after the seed.
prop_iterateNM :: Int -> Fun Int Int -> Int -> Property
prop_iterateNM n (Fn f) a =
    fmap (fmap elems) (NEV.iterateNM n (\b -> logged b (f b)) a)
      === (List.take (n - 1) as, whenNonEmpty (List.take n as))
  where
    as = List.iterate f a

prop_iterateN1M :: Int -> Fun Int Int -> Int -> Property
prop_iterateN1M n (Fn f) a =
    fmap elems (NEV.iterateN1M n (\b -> logged b (f b)) a)
      === (List.take (n' - 1) as, List.take n' as)
  where
    n' = max n 1
    as = List.iterate f a

prop_create :: [Int] -> Property
prop_create as =
    fmap elems (NEV.create (V.thaw (V.fromList as))) === whenNonEmpty as

prop_unsafeCreate :: NonEmptyVector Int -> Property
prop_unsafeCreate v =
    elems (NEV.unsafeCreate (V.thaw (V.fromList (elems v)))) === elems v

prop_createT :: [[Int]] -> Property
prop_createT ass =
    fmap (fmap elems) (NEV.createT (traverse (V.thaw . V.fromList) ass))
      === fmap whenNonEmpty ass

prop_unsafeCreateT :: [NonEmptyVector Int] -> Property
prop_unsafeCreateT vs =
    fmap elems (NEV.unsafeCreateT (traverse (V.thaw . V.fromList . elems) vs))
      === fmap elems vs

-- ---------------------------------------------------------------------- --
-- Unfolding

prop_unfoldr :: Fun Int Int -> Int -> Property
prop_unfoldr (Fn f) b =
    fmap elems (NEV.unfoldr (unfoldStep f) b)
      === whenNonEmpty (List.unfoldr (unfoldStep f) b)

prop_unfoldr1 :: Fun Int Int -> Int -> Int -> Property
prop_unfoldr1 (Fn f) a b =
    elems (NEV.unfoldr1 (unfoldStep f) a b)
      === a : List.unfoldr (unfoldStep f) b

prop_unfoldrN :: Int -> Fun Int Int -> Int -> Property
prop_unfoldrN n (Fn f) b =
    fmap elems (NEV.unfoldrN n (unfoldStep f) b)
      === whenNonEmpty (List.take n (List.unfoldr (unfoldStep f) b))

-- The initial element is supplied in addition to at most @n@ unfolded ones.
prop_unfoldr1N :: Int -> Fun Int Int -> Int -> Int -> Property
prop_unfoldr1N n (Fn f) a b =
    elems (NEV.unfoldr1N n (unfoldStep f) a b)
      === a : List.take n (List.unfoldr (unfoldStep f) b)

prop_unfoldrM :: Fun Int Int -> Int -> Property
prop_unfoldrM (Fn f) b =
    fmap (fmap elems) (NEV.unfoldrM step b)
      === fmap whenNonEmpty (unfoldrMList step b)
  where
    step s = logged s (unfoldStep f s)

prop_unfoldr1M :: Fun Int Int -> Int -> Int -> Property
prop_unfoldr1M (Fn f) a b =
    fmap elems (NEV.unfoldr1M step a b) === fmap (a :) (unfoldrMList step b)
  where
    step s = logged s (unfoldStep f s)

prop_unfoldrNM :: Int -> Fun Int Int -> Int -> Property
prop_unfoldrNM n (Fn f) b =
    fmap (fmap elems) (NEV.unfoldrNM n step b)
      === fmap whenNonEmpty (unfoldrNMList n step b)
  where
    step s = logged s (unfoldStep f s)

prop_unfoldr1NM :: Int -> Fun Int Int -> Int -> Int -> Property
prop_unfoldr1NM n (Fn f) a b =
    fmap elems (NEV.unfoldr1NM n step a b)
      === fmap (a :) (unfoldrNMList n step b)
  where
    step s = logged s (unfoldStep f s)

-- Each element is generated from the prefix constructed so far.
prop_constructN :: Fun [Int] Int -> Property
prop_constructN (Fn f) = forAll (choose (0, 30)) $ \n ->
    fmap elems (NEV.constructN n (f . V.toList))
      === whenNonEmpty (construct n [])
  where
    construct n acc
      | List.length acc >= n = acc
      | otherwise = construct n (acc ++ [f acc])

-- Each element is generated from the suffix constructed so far.
prop_constructrN :: Fun [Int] Int -> Property
prop_constructrN (Fn f) = forAll (choose (0, 30)) $ \n ->
    fmap elems (NEV.constructrN n (f . V.toList))
      === whenNonEmpty (construct n [])
  where
    construct n acc
      | List.length acc >= n = acc
      | otherwise = construct n (f acc : acc)

-- ---------------------------------------------------------------------- --
-- Enumeration

prop_enumFromN :: Int -> Int -> Property
prop_enumFromN a n =
    fmap elems (NEV.enumFromN a n) === whenNonEmpty (fmap (a +) [0 .. n - 1])

prop_enumFromN1 :: Int -> Int -> Property
prop_enumFromN1 a n =
    elems (NEV.enumFromN1 a n) === fmap (a +) [0 .. max n 1 - 1]

prop_enumFromStepN :: Int -> Int -> Int -> Property
prop_enumFromStepN a s n =
    fmap elems (NEV.enumFromStepN a s n)
      === whenNonEmpty [ a + s * i | i <- [0 .. n - 1] ]

prop_enumFromStepN1 :: Int -> Int -> Int -> Property
prop_enumFromStepN1 a s n =
    elems (NEV.enumFromStepN1 a s n) === [ a + s * i | i <- [0 .. max n 1 - 1] ]

prop_enumFromTo :: Int -> Int -> Property
prop_enumFromTo a b = fmap elems (NEV.enumFromTo a b) === whenNonEmpty [a .. b]

-- A zero step would enumerate forever.
prop_enumFromThenTo :: Int -> Int -> Int -> Property
prop_enumFromThenTo a b c = a /= b ==>
    fmap elems (NEV.enumFromThenTo a b c) === whenNonEmpty [a, b .. c]

-- ---------------------------------------------------------------------- --
-- Concatenation

prop_cons :: Int -> NonEmptyVector Int -> Property
prop_cons a v = elems (NEV.cons a v) === a : elems v

prop_consV :: Int -> [Int] -> Property
prop_consV a as = elems (NEV.consV a (V.fromList as)) === a : as

prop_snoc :: NonEmptyVector Int -> Int -> Property
prop_snoc v a = elems (NEV.snoc v a) === elems v ++ [a]

prop_snocV :: [Int] -> Int -> Property
prop_snocV as a = elems (NEV.snocV (V.fromList as) a) === as ++ [a]

prop_append :: NonEmptyVector Int -> NonEmptyVector Int -> Property
prop_append v w = elems (v NEV.++ w) === elems v ++ elems w

prop_concat :: [NonEmptyVector Int] -> Property
prop_concat vs =
    fmap elems (NEV.concat vs) === whenNonEmpty (List.concatMap elems vs)

prop_concat1 :: NonEmptyVector Int -> [NonEmptyVector Int] -> Property
prop_concat1 v vs =
    elems (NEV.concat1 (v :| vs)) === List.concatMap elems (v : vs)

-- ---------------------------------------------------------------------- --
-- Restricting memory usage

prop_force :: NonEmptyVector Int -> Property
prop_force v = forAll (genSlice v) $ \(i, m) -> m > 0 ==>
    let w = NonEmptyVector (V.slice i m (_neVec v))
    in elems (NEV.force w) === List.take m (List.drop i (elems v))
