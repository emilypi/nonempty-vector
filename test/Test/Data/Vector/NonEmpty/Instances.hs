{-# LANGUAGE CPP #-}
-- |
-- Module      : Test.Data.Vector.NonEmpty.Instances
-- Copyright   : (c) 2019-2026 Emily Pillmore
-- License     : BSD-style
--
-- Maintainer  : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability   : experimental
-- Portability : non-portable
--
-- Laws and list models for the type class instances of t'NonEmptyVector'.
--
module Test.Data.Vector.NonEmpty.Instances
( tests
) where


import qualified Control.Applicative as Applicative
import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import Control.Monad.Zip (munzip, mzip, mzipWith)

import Data.Data (gmapM, gmapQ, gmapT, showConstr, toConstr)
import qualified Data.Foldable as Foldable
#if MIN_VERSION_base(4,18,0)
import qualified Data.Foldable1 as Foldable1
#endif
import Data.Functor.Classes (liftCompare, liftEq, readsPrec1, showsPrec1)
import Data.Functor.Compose (Compose(..))
import Data.Functor.Identity (Identity(..))
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty(..))
import Data.Ord (comparing)
import Data.Semigroup (sconcat, stimes)
import Data.Vector.NonEmpty (NonEmptyVector)
import qualified Data.Vector.NonEmpty as NEV

import Test.Data.Vector.NonEmpty.Util
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

import Text.Read (readMaybe)


tests :: TestTree
tests = testGroup "Instances"
    [ testGroup "Eq"
      [ testProperty "prop_eq" prop_eq
      , testProperty "prop_liftEq" prop_liftEq
      ]
    , testGroup "Ord"
      [ testProperty "prop_compare" prop_compare
      , testProperty "prop_liftCompare" prop_liftCompare
      ]
    , testGroup "Show"
      [ testProperty "prop_show" prop_show
      , testProperty "prop_showsPrec" prop_showsPrec
      , testProperty "prop_showsPrec1" prop_showsPrec1
      ]
    , testGroup "Read"
      [ testProperty "prop_read_show" prop_read_show
      , testProperty "prop_read_list" prop_read_list
      , testProperty "prop_readsPrec1" prop_readsPrec1
      ]
    , testGroup "Functor"
      [ testProperty "prop_fmap" prop_fmap
      , testProperty "prop_functor_identity" prop_functor_identity
      , testProperty "prop_functor_composition" prop_functor_composition
      ]
    , testGroup "Applicative"
      [ testProperty "prop_pure" prop_pure
      , testProperty "prop_ap" prop_ap
      , testProperty "prop_liftA2" prop_liftA2
      , testProperty "prop_then" prop_then
      , testProperty "prop_before" prop_before
      , testProperty "prop_applicative_identity" prop_applicative_identity
      , testProperty "prop_applicative_homomorphism" prop_applicative_homomorphism
      , testProperty "prop_applicative_interchange" prop_applicative_interchange
      , testProperty "prop_applicative_composition" prop_applicative_composition
      ]
    , testGroup "Monad"
      [ testProperty "prop_bind" prop_bind
      , testProperty "prop_monad_left_identity" prop_monad_left_identity
      , testProperty "prop_monad_right_identity" prop_monad_right_identity
      , testProperty "prop_monad_associativity" prop_monad_associativity
      ]
    , testGroup "MonadZip"
      [ testProperty "prop_mzip" prop_mzip
      , testProperty "prop_mzipWith" prop_mzipWith
      , testProperty "prop_munzip" prop_munzip
      , testProperty "prop_monadzip_information" prop_monadzip_information
      ]
    , testGroup "Semigroup"
      [ testProperty "prop_append" prop_append
      , testProperty "prop_semigroup_associativity" prop_semigroup_associativity
      , testProperty "prop_sconcat" prop_sconcat
      , testProperty "prop_stimes" prop_stimes
      , testProperty "prop_stimes_non_positive" prop_stimes_non_positive
      ]
    , testGroup "Foldable"
      [ testProperty "prop_foldable_toList" prop_foldable_toList
      , testProperty "prop_foldable_foldMap" prop_foldable_foldMap
      , testProperty "prop_foldable_foldr" prop_foldable_foldr
      , testProperty "prop_foldable_foldl" prop_foldable_foldl
      , testProperty "prop_foldable_length" prop_foldable_length
      , testProperty "prop_foldable_null" prop_foldable_null
      , testProperty "prop_foldable_elem" prop_foldable_elem
      , testProperty "prop_foldable_sum_product" prop_foldable_sum_product
      , testProperty "prop_foldable_maximum_minimum" prop_foldable_maximum_minimum
      ]
#if MIN_VERSION_base(4,18,0)
    , testGroup "Foldable1"
      [ testProperty "prop_foldable1_foldMap1" prop_foldable1_foldMap1
      , testProperty "prop_foldable1_foldMap1'" prop_foldable1_foldMap1'
      , testProperty "prop_foldable1_fold1" prop_foldable1_fold1
      , testProperty "prop_foldable1_toNonEmpty" prop_foldable1_toNonEmpty
      , testProperty "prop_foldable1_head_last" prop_foldable1_head_last
      , testProperty "prop_foldable1_maximum_minimum" prop_foldable1_maximum_minimum
      , testProperty "prop_foldable1_foldrMap1" prop_foldable1_foldrMap1
      , testProperty "prop_foldable1_foldlMap1" prop_foldable1_foldlMap1
      ]
#endif
    , testGroup "Traversable"
      [ testProperty "prop_traverse" prop_traverse
      , testProperty "prop_sequenceA" prop_sequenceA
      , testProperty "prop_traversable_identity" prop_traversable_identity
      , testProperty "prop_traversable_composition" prop_traversable_composition
      ]
    , testGroup "NFData"
      [ testProperty "prop_rnf" prop_rnf
      , testProperty "prop_rnf_forces_elements" prop_rnf_forces_elements
      ]
    , testGroup "Data"
      [ testProperty "prop_data" prop_data
      ]
    ]

type V = NonEmptyVector Int

-- | Either the same vector or an unrelated one, so that equal pairs are
-- generated often.
--
genNear :: V -> Gen V
genNear v = oneof [pure v, arbitrary]

-- ---------------------------------------------------------------------- --
-- Eq and Ord

prop_eq :: V -> Property
prop_eq v = forAll (genNear v) $ \w -> (v == w) === (elems v == elems w)

prop_liftEq :: V -> Property
prop_liftEq v = forAll (genNear v) $ \w ->
    liftEq eqMod3 v w === liftEq eqMod3 (elems v) (elems w)
  where
    eqMod3 a b = a `mod` 3 == b `mod` 3

prop_compare :: V -> Property
prop_compare v = forAll (genNear v) $ \w ->
    compare v w === compare (elems v) (elems w)

prop_liftCompare :: V -> Property
prop_liftCompare v = forAll (genNear v) $ \w ->
    liftCompare cmp v w === liftCompare cmp (elems v) (elems w)
  where
    cmp = comparing (`mod` 3)

-- ---------------------------------------------------------------------- --
-- Show and Read

prop_show :: V -> Property
prop_show v = show v === show (elems v)

prop_showsPrec :: Int -> V -> String -> Property
prop_showsPrec d v s = showsPrec d v s === show (elems v) ++ s

prop_showsPrec1 :: Int -> V -> String -> Property
prop_showsPrec1 d v s = showsPrec1 d v s === showsPrec1 d (elems v) s

prop_read_show :: V -> Property
prop_read_show v = read (show v) === v

-- The empty list must not parse.
prop_read_list :: [Int] -> Property
prop_read_list as =
    fmap elems (readMaybe (show as) :: Maybe V) === whenNonEmpty as

prop_readsPrec1 :: [Int] -> Property
prop_readsPrec1 as =
    [ (elems v, rest) | (v, rest) <- readsPrec1 0 (show as) :: [(V, String)] ]
      === [ (as, "") | not (List.null as) ]

-- ---------------------------------------------------------------------- --
-- Functor

prop_fmap :: Fun Int Int -> V -> Property
prop_fmap (Fn f) v = elems (fmap f v) === List.map f (elems v)

prop_functor_identity :: V -> Property
prop_functor_identity v = fmap id v === v

prop_functor_composition :: Fun Int Int -> Fun Int Int -> V -> Property
prop_functor_composition (Fn f) (Fn g) v =
    fmap (f . g) v === (fmap f . fmap g) v

-- ---------------------------------------------------------------------- --
-- Applicative
--
-- '<*>' and '>>=' multiply sizes, so laws chaining several of them run on
-- small inputs.

prop_pure :: Int -> Property
prop_pure a = elems (pure a :: V) === [a]

prop_ap :: NonEmptyVector (Fun Int Int) -> V -> Property
prop_ap fs v =
    elems (fmap applyFun fs <*> v)
      === (List.map applyFun (elems fs) <*> elems v)

prop_liftA2 :: Fun (Int, Int) Int -> V -> V -> Property
prop_liftA2 (Fn2 f) v w =
    elems (Applicative.liftA2 f v w)
      === Applicative.liftA2 f (elems v) (elems w)

prop_then :: V -> V -> Property
prop_then v w = elems (v *> w) === (elems v *> elems w)

prop_before :: V -> V -> Property
prop_before v w = elems (v <* w) === (elems v <* elems w)

prop_applicative_identity :: V -> Property
prop_applicative_identity v = (pure id <*> v) === v

prop_applicative_homomorphism :: Fun Int Int -> Int -> Property
prop_applicative_homomorphism (Fn f) a = (pure f <*> pure a) === (pure (f a) :: V)

prop_applicative_interchange :: NonEmptyVector (Fun Int Int) -> Int -> Property
prop_applicative_interchange fs a = (u <*> pure a) === (pure ($ a) <*> u)
  where
    u = fmap applyFun fs

prop_applicative_composition :: Property
prop_applicative_composition = forAll (resize 10 arbitrary) $ \(fs, gs, w) ->
    let u = fmap applyFun (fs :: NonEmptyVector (Fun Int Int))
        v = fmap applyFun (gs :: NonEmptyVector (Fun Int Int))
    in (pure (.) <*> u <*> v <*> (w :: V)) === (u <*> (v <*> w))

-- ---------------------------------------------------------------------- --
-- Monad

prop_bind :: V -> Fun Int V -> Property
prop_bind v (Fn f) = elems (v >>= f) === (elems v >>= elems . f)

prop_monad_left_identity :: Int -> Fun Int V -> Property
prop_monad_left_identity a (Fn f) = (pure a >>= f) === f a

prop_monad_right_identity :: V -> Property
prop_monad_right_identity v = (v >>= pure) === v

prop_monad_associativity :: Property
prop_monad_associativity = forAll (resize 10 arbitrary) $ \(v, Fn f, Fn g) ->
    ((v :: V) >>= f >>= g) === (v >>= (\a -> f a >>= (g :: Int -> V)))

-- ---------------------------------------------------------------------- --
-- MonadZip

prop_mzip :: V -> V -> Property
prop_mzip v w = elems (mzip v w) === List.zip (elems v) (elems w)

prop_mzipWith :: Fun (Int, Int) Int -> V -> V -> Property
prop_mzipWith (Fn2 f) v w =
    elems (mzipWith f v w) === List.zipWith f (elems v) (elems w)

prop_munzip :: NonEmptyVector (Int, Int) -> Property
prop_munzip v = (elems a, elems b) === List.unzip (elems v)
  where
    (a, b) = munzip v

-- Zipping vectors of equal shape loses no information.
prop_monadzip_information :: Fun Int Int -> V -> Property
prop_monadzip_information (Fn f) v = munzip (mzip v w) === (v, w)
  where
    w = fmap f v

-- ---------------------------------------------------------------------- --
-- Semigroup

prop_append :: V -> V -> Property
prop_append v w = elems (v <> w) === elems v ++ elems w

prop_semigroup_associativity :: V -> V -> V -> Property
prop_semigroup_associativity u v w = ((u <> v) <> w) === (u <> (v <> w))

prop_sconcat :: V -> [V] -> Property
prop_sconcat v vs = elems (sconcat (v :| vs)) === List.concatMap elems (v : vs)

prop_stimes :: V -> Property
prop_stimes v = forAll (choose (1, 5)) $ \n ->
    elems (stimes (n :: Int) v) === List.concat (List.replicate n (elems v))

-- A non-positive multiplier would produce an empty vector, so it must throw.
prop_stimes_non_positive :: V -> Property
prop_stimes_non_positive v = forAll (choose (-5, 0)) $ \n ->
    throws (stimes (n :: Int) v)

-- ---------------------------------------------------------------------- --
-- Foldable
--
-- Order-sensitive monoids and fold functions pin down traversal order.

prop_foldable_toList :: V -> Property
prop_foldable_toList v = Foldable.toList v === elems v

prop_foldable_foldMap :: Fun Int [Int] -> V -> Property
prop_foldable_foldMap (Fn f) v = Foldable.foldMap f v === Foldable.foldMap f (elems v)

prop_foldable_foldr :: Fun (Int, Int) Int -> Int -> V -> Property
prop_foldable_foldr (Fn2 f) z v =
    Foldable.foldr f z v === Foldable.foldr f z (elems v)

prop_foldable_foldl :: Fun (Int, Int) Int -> Int -> V -> Property
prop_foldable_foldl (Fn2 f) z v =
    Foldable.foldl f z v === Foldable.foldl f z (elems v)

prop_foldable_length :: V -> Property
prop_foldable_length v = Foldable.length v === List.length (elems v)

prop_foldable_null :: V -> Property
prop_foldable_null v = Foldable.null v === False

prop_foldable_elem :: Int -> V -> Property
prop_foldable_elem a v =
    Foldable.elem (a `mod` 4) w === Foldable.elem (a `mod` 4) (elems w)
  where
    w = vec (List.map (`mod` 4) (elems v))

prop_foldable_sum_product :: V -> Property
prop_foldable_sum_product v =
    (Foldable.sum v, Foldable.product v)
      === (Foldable.sum (elems v), Foldable.product (elems v))

prop_foldable_maximum_minimum :: V -> Property
prop_foldable_maximum_minimum v =
    (Foldable.maximum v, Foldable.minimum v)
      === (Foldable.maximum (elems1 v), Foldable.minimum (elems1 v))

-- ---------------------------------------------------------------------- --
-- Foldable1
--
-- The instance for non-empty lists serves as the model.

#if MIN_VERSION_base(4,18,0)
prop_foldable1_foldMap1 :: Fun Int [Int] -> V -> Property
prop_foldable1_foldMap1 (Fn f) v =
    Foldable1.foldMap1 f v === Foldable1.foldMap1 f (elems1 v)

prop_foldable1_foldMap1' :: Fun Int [Int] -> V -> Property
prop_foldable1_foldMap1' (Fn f) v =
    Foldable1.foldMap1' f v === Foldable1.foldMap1' f (elems1 v)

prop_foldable1_fold1 :: NonEmptyVector [Int] -> Property
prop_foldable1_fold1 v = Foldable1.fold1 v === Foldable1.fold1 (elems1 v)

prop_foldable1_toNonEmpty :: V -> Property
prop_foldable1_toNonEmpty v = Foldable1.toNonEmpty v === elems1 v

prop_foldable1_head_last :: V -> Property
prop_foldable1_head_last v =
    (Foldable1.head v, Foldable1.last v)
      === (Foldable1.head (elems1 v), Foldable1.last (elems1 v))

prop_foldable1_maximum_minimum :: V -> Property
prop_foldable1_maximum_minimum v =
    (Foldable1.maximum v, Foldable1.minimum v)
      === (Foldable1.maximum (elems1 v), Foldable1.minimum (elems1 v))

prop_foldable1_foldrMap1 :: Fun Int Int -> Fun (Int, Int) Int -> V -> Property
prop_foldable1_foldrMap1 (Fn f) (Fn2 g) v =
    (Foldable1.foldrMap1 f g v, Foldable1.foldrMap1' f g v)
      === ( Foldable1.foldrMap1 f g (elems1 v)
          , Foldable1.foldrMap1' f g (elems1 v)
          )

prop_foldable1_foldlMap1 :: Fun Int Int -> Fun (Int, Int) Int -> V -> Property
prop_foldable1_foldlMap1 (Fn f) (Fn2 g) v =
    (Foldable1.foldlMap1 f g v, Foldable1.foldlMap1' f g v)
      === ( Foldable1.foldlMap1 f g (elems1 v)
          , Foldable1.foldlMap1' f g (elems1 v)
          )
#endif

-- ---------------------------------------------------------------------- --
-- Traversable

prop_traverse :: Fun Int Int -> V -> Property
prop_traverse (Fn f) v =
    fmap elems (traverse (\a -> logged a (f a)) v)
      === traverse (\a -> logged a (f a)) (elems v)

prop_sequenceA :: NonEmptyVector ([Int], Int) -> Property
prop_sequenceA v = fmap elems (sequenceA v) === sequenceA (elems v)

prop_traversable_identity :: V -> Property
prop_traversable_identity v = traverse Identity v === Identity v

prop_traversable_composition
    :: Fun Int (Maybe Int)
    -> Fun Int ([Int], Int)
    -> V
    -> Property
prop_traversable_composition (Fn f) (Fn g) v =
    traverse (Compose . fmap g . f) v
      === (Compose . fmap (traverse g) . traverse f) v

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
      .&&. showConstr (toConstr v) === "NonEmptyVector"
