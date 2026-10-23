-- |
-- Module      : Test.Data.Vector.NonEmpty.Elementwise
-- Copyright   : (c) 2019-2026 Emily Pillmore
-- License     : BSD-style
--
-- Maintainer  : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability   : experimental
-- Portability : non-portable
--
-- Properties for indexing, mapping, zipping, and unzipping.
--
-- Monadic operations run in a writer-style @([w], -)@ monad so that the
-- order and number of effects are observable.
--
module Test.Data.Vector.NonEmpty.Elementwise
( tests
) where


import qualified Data.List as List
import Data.Vector.NonEmpty (NonEmptyVector)
import qualified Data.Vector.NonEmpty as NEV

import Test.Data.Vector.NonEmpty.Util
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck


tests :: TestTree
tests = testGroup "Elementwise"
    [ testGroup "Indexing"
      [ testProperty "prop_indexed" prop_indexed
      ]
    , testGroup "Mapping"
      [ testProperty "prop_map" prop_map
      , testProperty "prop_imap" prop_imap
      , testProperty "prop_concatMap" prop_concatMap
      ]
    , testGroup "Monadic mapping"
      [ testProperty "prop_mapM" prop_mapM
      , testProperty "prop_imapM" prop_imapM
      , testProperty "prop_mapM_" prop_mapM_
      , testProperty "prop_imapM_" prop_imapM_
      , testProperty "prop_forM" prop_forM
      , testProperty "prop_forM_" prop_forM_
      ]
    , testGroup "Zipping"
      [ testProperty "prop_zipWith" prop_zipWith
      , testProperty "prop_zipWith3" prop_zipWith3
      , testProperty "prop_zipWith4" prop_zipWith4
      , testProperty "prop_zipWith5" prop_zipWith5
      , testProperty "prop_zipWith6" prop_zipWith6
      , testProperty "prop_izipWith" prop_izipWith
      , testProperty "prop_izipWith3" prop_izipWith3
      , testProperty "prop_izipWith4" prop_izipWith4
      , testProperty "prop_izipWith5" prop_izipWith5
      , testProperty "prop_izipWith6" prop_izipWith6
      , testProperty "prop_zip" prop_zip
      , testProperty "prop_zip3" prop_zip3
      , testProperty "prop_zip4" prop_zip4
      , testProperty "prop_zip5" prop_zip5
      , testProperty "prop_zip6" prop_zip6
      ]
    , testGroup "Monadic zipping"
      [ testProperty "prop_zipWithM" prop_zipWithM
      , testProperty "prop_zipWithM_" prop_zipWithM_
      , testProperty "prop_izipWithM" prop_izipWithM
      , testProperty "prop_izipWithM_" prop_izipWithM_
      ]
    , testGroup "Unzipping"
      [ testProperty "prop_unzip" prop_unzip
      , testProperty "prop_unzip3" prop_unzip3
      , testProperty "prop_unzip4" prop_unzip4
      , testProperty "prop_unzip5" prop_unzip5
      , testProperty "prop_unzip6" prop_unzip6
      ]
    ]

type V = NonEmptyVector Int

-- ---------------------------------------------------------------------- --
-- Indexing

prop_indexed :: V -> Property
prop_indexed v = elems (NEV.indexed v) === List.zip [0 ..] (elems v)

-- ---------------------------------------------------------------------- --
-- Mapping

prop_map :: Fun Int Int -> V -> Property
prop_map (Fn f) v = elems (NEV.map f v) === List.map f (elems v)

prop_imap :: Fun (Int, Int) Int -> V -> Property
prop_imap (Fn2 f) v = elems (NEV.imap f v) === List.zipWith f [0 ..] (elems v)

prop_concatMap :: Fun Int V -> V -> Property
prop_concatMap (Fn f) v =
    elems (NEV.concatMap f v) === List.concatMap (elems . f) (elems v)

-- ---------------------------------------------------------------------- --
-- Monadic mapping

prop_mapM :: Fun Int Int -> V -> Property
prop_mapM (Fn f) v =
    fmap elems (NEV.mapM (\a -> logged a (f a)) v)
      === (elems v, List.map f (elems v))

prop_imapM :: Fun (Int, Int) Int -> V -> Property
prop_imapM (Fn2 f) v =
    fmap elems (NEV.imapM (\i a -> logged i (f i a)) v)
      === (is, List.zipWith f is (elems v))
  where
    is = [0 .. NEV.length v - 1]

prop_mapM_ :: V -> Property
prop_mapM_ v = NEV.mapM_ (\a -> logged a a) v === (elems v, ())

prop_imapM_ :: V -> Property
prop_imapM_ v =
    NEV.imapM_ (\i a -> logged (i, a) a) v
      === (List.zip [0 ..] (elems v), ())

prop_forM :: Fun Int Int -> V -> Property
prop_forM (Fn f) v =
    fmap elems (NEV.forM v (\a -> logged a (f a)))
      === (elems v, List.map f (elems v))

prop_forM_ :: V -> Property
prop_forM_ v = NEV.forM_ v (\a -> logged a a) === (elems v, ())

-- ---------------------------------------------------------------------- --
-- Zipping
--
-- The higher arities collect their arguments in order, so the models
-- detect both truncation to the shortest input and argument order.

prop_zipWith :: Fun (Int, Int) Int -> V -> V -> Property
prop_zipWith (Fn2 f) a b =
    elems (NEV.zipWith f a b) === List.zipWith f (elems a) (elems b)

prop_zipWith3 :: Fun (Int, Int, Int) Int -> V -> V -> V -> Property
prop_zipWith3 (Fn3 f) a b c =
    elems (NEV.zipWith3 f a b c)
      === List.zipWith3 f (elems a) (elems b) (elems c)

prop_zipWith4 :: V -> V -> V -> V -> Property
prop_zipWith4 a b c d =
    elems (NEV.zipWith4 k a b c d)
      === List.zipWith4 k (elems a) (elems b) (elems c) (elems d)
  where
    k p q r s = [p, q, r, s]

prop_zipWith5 :: V -> V -> V -> V -> V -> Property
prop_zipWith5 a b c d e =
    elems (NEV.zipWith5 k a b c d e)
      === List.zipWith5 k (elems a) (elems b) (elems c) (elems d) (elems e)
  where
    k p q r s t = [p, q, r, s, t]

prop_zipWith6 :: V -> V -> V -> V -> V -> V -> Property
prop_zipWith6 a b c d e f =
    elems (NEV.zipWith6 k a b c d e f)
      === List.zipWith6 k
            (elems a) (elems b) (elems c) (elems d) (elems e) (elems f)
  where
    k p q r s t u = [p, q, r, s, t, u]

prop_izipWith :: V -> V -> Property
prop_izipWith a b =
    elems (NEV.izipWith k a b) === List.zipWith3 k [0 ..] (elems a) (elems b)
  where
    k i p q = [i, p, q]

prop_izipWith3 :: V -> V -> V -> Property
prop_izipWith3 a b c =
    elems (NEV.izipWith3 k a b c)
      === List.zipWith4 k [0 ..] (elems a) (elems b) (elems c)
  where
    k i p q r = [i, p, q, r]

prop_izipWith4 :: V -> V -> V -> V -> Property
prop_izipWith4 a b c d =
    elems (NEV.izipWith4 k a b c d)
      === List.zipWith5 k [0 ..] (elems a) (elems b) (elems c) (elems d)
  where
    k i p q r s = [i, p, q, r, s]

prop_izipWith5 :: V -> V -> V -> V -> V -> Property
prop_izipWith5 a b c d e =
    elems (NEV.izipWith5 k a b c d e)
      === List.zipWith6 k [0 ..]
            (elems a) (elems b) (elems c) (elems d) (elems e)
  where
    k i p q r s t = [i, p, q, r, s, t]

prop_izipWith6 :: V -> V -> V -> V -> V -> V -> Property
prop_izipWith6 a b c d e f =
    elems (NEV.izipWith6 k a b c d e f)
      === List.zipWith7 k [0 ..]
            (elems a) (elems b) (elems c) (elems d) (elems e) (elems f)
  where
    k i p q r s t u = [i, p, q, r, s, t, u]

prop_zip :: V -> V -> Property
prop_zip a b = elems (NEV.zip a b) === List.zip (elems a) (elems b)

prop_zip3 :: V -> V -> V -> Property
prop_zip3 a b c =
    elems (NEV.zip3 a b c) === List.zip3 (elems a) (elems b) (elems c)

prop_zip4 :: V -> V -> V -> V -> Property
prop_zip4 a b c d =
    elems (NEV.zip4 a b c d)
      === List.zip4 (elems a) (elems b) (elems c) (elems d)

prop_zip5 :: V -> V -> V -> V -> V -> Property
prop_zip5 a b c d e =
    elems (NEV.zip5 a b c d e)
      === List.zip5 (elems a) (elems b) (elems c) (elems d) (elems e)

prop_zip6 :: V -> V -> V -> V -> V -> V -> Property
prop_zip6 a b c d e f =
    elems (NEV.zip6 a b c d e f)
      === List.zip6 (elems a) (elems b) (elems c) (elems d) (elems e) (elems f)

-- ---------------------------------------------------------------------- --
-- Monadic zipping

prop_zipWithM :: Fun (Int, Int) Int -> V -> V -> Property
prop_zipWithM (Fn2 f) a b =
    fmap elems (NEV.zipWithM (\p q -> logged (p, q) (f p q)) a b)
      === (ps, List.map (uncurry f) ps)
  where
    ps = List.zip (elems a) (elems b)

prop_zipWithM_ :: V -> V -> Property
prop_zipWithM_ a b =
    NEV.zipWithM_ (\p q -> logged (p, q) p) a b
      === (List.zip (elems a) (elems b), ())

prop_izipWithM :: Fun (Int, Int, Int) Int -> V -> V -> Property
prop_izipWithM (Fn3 f) a b =
    fmap elems (NEV.izipWithM (\i p q -> logged i (f i p q)) a b)
      === (List.map fst ips, List.map (\(i, (p, q)) -> f i p q) ips)
  where
    ips = List.zip [0 ..] (List.zip (elems a) (elems b))

prop_izipWithM_ :: V -> V -> Property
prop_izipWithM_ a b =
    NEV.izipWithM_ (\i p q -> logged (i, p, q) p) a b
      === (List.zip3 [0 ..] (elems a) (elems b), ())

-- ---------------------------------------------------------------------- --
-- Unzipping

prop_unzip :: NonEmptyVector (Int, Int) -> Property
prop_unzip v = (elems a, elems b) === List.unzip (elems v)
  where
    (a, b) = NEV.unzip v

prop_unzip3 :: NonEmptyVector (Int, Int, Int) -> Property
prop_unzip3 v = (elems a, elems b, elems c) === List.unzip3 (elems v)
  where
    (a, b, c) = NEV.unzip3 v

prop_unzip4 :: NonEmptyVector (Int, Int, Int, Int) -> Property
prop_unzip4 v =
    (elems a, elems b, elems c, elems d) === List.unzip4 (elems v)
  where
    (a, b, c, d) = NEV.unzip4 v

prop_unzip5 :: NonEmptyVector (Int, Int, Int, Int, Int) -> Property
prop_unzip5 v =
    (elems a, elems b, elems c, elems d, elems e) === List.unzip5 (elems v)
  where
    (a, b, c, d, e) = NEV.unzip5 v

prop_unzip6 :: NonEmptyVector (Int, Int, Int, Int, Int, Int) -> Property
prop_unzip6 v =
    (elems a, elems b, elems c, elems d, elems e, elems f)
      === List.unzip6 (elems v)
  where
    (a, b, c, d, e, f) = NEV.unzip6 v
