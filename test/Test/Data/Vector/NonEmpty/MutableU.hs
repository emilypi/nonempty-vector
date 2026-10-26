{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module      : Test.Data.Vector.NonEmpty.MutableU
-- Copyright   : (c) 2019-2026 Emily Pillmore, Koz Ross
-- License     : BSD-style
--
-- Maintainer  : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability   : experimental
-- Portability : non-portable
--
-- Properties for non-empty mutable vectors, observed by freezing them
-- into lists.
--
module Test.Data.Vector.NonEmpty.MutableU
( tests
) where


import Control.Monad.ST (ST, runST)

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import Data.STRef (modifySTRef', newSTRef, readSTRef)
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Vector.NonEmpty.InternalU
    ( NonEmptyMUVector(..)
    , NonEmptyUVector(..)
    , NonEmptyIOUVector
    , NonEmptySTUVector
    )
import qualified Data.Vector.Unboxed.NonEmpty.Mutable as NEMV

import Test.Data.Vector.NonEmpty.Util
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck


tests :: TestTree
tests = testGroup "Mutable (unboxed)"
    [ testGroup "Length information"
      [ testProperty "prop_length" prop_length
      ]
    , testGroup "Extracting subvectors"
      [ testProperty "prop_slice" prop_slice
      , testProperty "prop_slice_shares" prop_slice_shares
      , testProperty "prop_init" prop_init
      , testProperty "prop_tail" prop_tail
      , testProperty "prop_take" prop_take
      , testProperty "prop_drop" prop_drop
      , testProperty "prop_splitAt" prop_splitAt
      , testProperty "prop_unsafeSlice" prop_unsafeSlice
      , testProperty "prop_unsafeTake" prop_unsafeTake
      , testProperty "prop_unsafeDrop" prop_unsafeDrop
      ]
    , testGroup "Overlapping"
      [ testProperty "prop_overlaps_self" prop_overlaps_self
      , testProperty "prop_overlaps_distinct" prop_overlaps_distinct
      , testProperty "prop_overlaps_slices" prop_overlaps_slices
      ]
    , testGroup "Conversions"
      [ testProperty "prop_fromMVector" prop_fromMVector
      , testProperty "prop_toMVector" prop_toMVector
      , testProperty "prop_unsafeFromMVector" prop_unsafeFromMVector
      ]
    , testGroup "Initialisation"
      [ testProperty "prop_new" prop_new
      , testProperty "prop_new1" prop_new1
      , testProperty "prop_unsafeNew" prop_unsafeNew
      , testProperty "prop_replicate" prop_replicate
      , testProperty "prop_replicate1" prop_replicate1
      , testProperty "prop_replicateM" prop_replicateM
      , testProperty "prop_replicate1M" prop_replicate1M
      , testProperty "prop_clone" prop_clone
      ]
    , testGroup "Growing"
      [ testProperty "prop_grow" prop_grow
      , testProperty "prop_grow_negative" prop_grow_negative
      , testProperty "prop_unsafeGrow" prop_unsafeGrow
      ]
    , testGroup "Accessing individual elements"
      [ testProperty "prop_read" prop_read
      , testProperty "prop_read_out_of_bounds" prop_read_out_of_bounds
      , testProperty "prop_write" prop_write
      , testProperty "prop_write_out_of_bounds" prop_write_out_of_bounds
      , testProperty "prop_modify" prop_modify
      , testProperty "prop_swap" prop_swap
      , testProperty "prop_unsafeRead" prop_unsafeRead
      , testProperty "prop_unsafeWrite" prop_unsafeWrite
      , testProperty "prop_unsafeModify" prop_unsafeModify
      , testProperty "prop_unsafeSwap" prop_unsafeSwap
      ]
    , testGroup "Modifying vectors"
      [ testProperty "prop_nextPermutation" prop_nextPermutation
      , testProperty "prop_nextPermutation_last" prop_nextPermutation_last
      ]
    , testGroup "Filling and copying"
      [ testProperty "prop_set" prop_set
      , testProperty "prop_copy" prop_copy
      , testProperty "prop_copy_length_mismatch" prop_copy_length_mismatch
      , testProperty "prop_unsafeCopy" prop_unsafeCopy
      , testProperty "prop_move" prop_move
      , testProperty "prop_unsafeMove" prop_unsafeMove
      ]
    ]

type V = NonEmptyUVector Int

-- ---------------------------------------------------------------------- --
-- Running mutable vectors

thaw :: V.Unbox a => NonEmptyUVector a -> ST s (NonEmptySTUVector s a)
thaw = fmap NonEmptyMUVector . V.thaw . _neUvec

thawIO :: V.Unbox a => NonEmptyUVector a -> IO (NonEmptyIOUVector a)
thawIO = fmap NonEmptyMUVector . V.thaw . _neUvec

-- | Run a computation against a fresh mutable copy of the vector.
--
withThawed :: V.Unbox a => NonEmptyUVector a -> (forall s. NonEmptySTUVector s a -> ST s b) -> b
withThawed v k = runST (thaw v >>= k)

freezeList :: V.Unbox a => MVector s a -> ST s [a]
freezeList = fmap V.toList . V.freeze

freeze :: V.Unbox a => NonEmptySTUVector s a -> ST s [a]
freeze = freezeList . _nemUvec

-- | A mutable operation's effect on the vector's contents.
--
mutated :: V -> (forall s. NonEmptySTUVector s Int -> ST s ()) -> [Int]
mutated v k = withThawed v (\mv -> k mv >> freeze mv)

-- ---------------------------------------------------------------------- --
-- Length information

prop_length :: V -> Property
prop_length v = withThawed v (pure . NEMV.length) === List.length (elemsU v)

-- ---------------------------------------------------------------------- --
-- Extracting subvectors

prop_slice :: V -> Property
prop_slice v = forAll (genSliceU v) $ \(i, m) ->
    withThawed v (freezeList . NEMV.slice i m)
      === List.take m (List.drop i (elemsU v))

-- Subvectors are views: writes through them reach the parent vector.
prop_slice_shares :: V -> Int -> Property
prop_slice_shares v a = forAll (genIndexU v) $ \i ->
    mutated v (\mv -> MV.write (NEMV.drop i mv) 0 a)
      === adjustAt i (const a) (elemsU v)

prop_init :: V -> Property
prop_init v =
    withThawed v (freezeList . NEMV.init)
      === List.take (List.length (elemsU v) - 1) (elemsU v)

prop_tail :: V -> Property
prop_tail v = withThawed v (freezeList . NEMV.tail) === List.drop 1 (elemsU v)

prop_take :: Int -> V -> Property
prop_take n v =
    withThawed v (freezeList . NEMV.take n) === List.take n (elemsU v)

prop_drop :: Int -> V -> Property
prop_drop n v =
    withThawed v (freezeList . NEMV.drop n) === List.drop n (elemsU v)

prop_splitAt :: Int -> V -> Property
prop_splitAt n v =
    withThawed v (\mv -> let (as, bs) = NEMV.splitAt n mv
                         in (,) <$> freezeList as <*> freezeList bs)
      === List.splitAt n (elemsU v)

prop_unsafeSlice :: V -> Property
prop_unsafeSlice v = forAll (genSliceU v) $ \(i, m) ->
    withThawed v (freezeList . NEMV.unsafeSlice i m)
      === List.take m (List.drop i (elemsU v))

prop_unsafeTake :: V -> Property
prop_unsafeTake v = forAll (choose (0, List.length (elemsU v))) $ \n ->
    withThawed v (freezeList . NEMV.unsafeTake n) === List.take n (elemsU v)

prop_unsafeDrop :: V -> Property
prop_unsafeDrop v = forAll (choose (0, List.length (elemsU v))) $ \n ->
    withThawed v (freezeList . NEMV.unsafeDrop n) === List.drop n (elemsU v)

-- ---------------------------------------------------------------------- --
-- Overlapping

prop_overlaps_self :: V -> Property
prop_overlaps_self v = withThawed v (\mv -> pure (NEMV.overlaps mv mv)) === True

prop_overlaps_distinct :: V -> V -> Property
prop_overlaps_distinct v w = runST (NEMV.overlaps <$> thaw v <*> thaw w) === False

-- Adjacent slices are disjoint; extending one by an element overlaps them.
prop_overlaps_slices :: V -> Property
prop_overlaps_slices v = n >= 2 ==> forAll (choose (1, n - 1)) $ \k ->
    withThawed v (\(NonEmptyMUVector mv) ->
      let front m = NonEmptyMUVector (MV.take m mv)
          back = NonEmptyMUVector (MV.drop k mv)
      in pure (NEMV.overlaps (front k) back, NEMV.overlaps (front (k + 1)) back))
      === (False, True)
  where
    n = List.length (elemsU v)

-- ---------------------------------------------------------------------- --
-- Conversions

prop_fromMVector :: [Int] -> Property
prop_fromMVector as =
    runST (V.thaw (V.fromList as) >>= traverse freeze . NEMV.fromMVector)
      === whenNonEmpty as

prop_toMVector :: V -> Property
prop_toMVector v = withThawed v (freezeList . NEMV.toMVector) === elemsU v

prop_unsafeFromMVector :: V -> Property
prop_unsafeFromMVector v =
    runST (V.thaw (_neUvec v) >>= freeze . NEMV.unsafeFromMVector) === elemsU v

-- ---------------------------------------------------------------------- --
-- Initialisation

prop_new :: NonNegative Int -> Property
prop_new (NonNegative n) =
    runST (fmap NEMV.length <$> NEMV.new @_ @Int n)
      === if n > 0 then Just n else Nothing

prop_new1 :: Int -> Property
prop_new1 n = runST (NEMV.length <$> NEMV.new1 @_ @Int n) === max n 1

prop_unsafeNew :: NonNegative Int -> Property
prop_unsafeNew (NonNegative n) =
    runST (fmap NEMV.length <$> NEMV.unsafeNew @_ @Int n)
      === if n > 0 then Just n else Nothing

prop_replicate :: Int -> Int -> Property
prop_replicate n a =
    runST (NEMV.replicate n a >>= traverse freeze)
      === whenNonEmpty (List.replicate n a)

prop_replicate1 :: Int -> Int -> Property
prop_replicate1 n a =
    runST (NEMV.replicate1 n a >>= freeze) === List.replicate (max n 1) a

-- The action runs once per element, in order.
prop_replicateM :: Int -> Property
prop_replicateM n =
    runST (counter >>= NEMV.replicateM n >>= traverse freeze)
      === whenNonEmpty [1 .. n]

prop_replicate1M :: Int -> Property
prop_replicate1M n =
    runST (counter >>= NEMV.replicate1M n >>= traverse freeze)
      === Just [1 .. max n 1]

-- | An action yielding 1, 2, 3, ... on successive runs.
--
counter :: ST s (ST s Int)
counter = do
    ref <- newSTRef 0
    pure (modifySTRef' ref (+ 1) >> readSTRef ref)

-- A clone has the same contents but no longer aliases the original.
prop_clone :: V -> Int -> Property
prop_clone v a =
    withThawed v (\mv -> do
      c <- NEMV.clone mv
      MV.set (_nemUvec c) a
      (,) <$> freeze mv <*> freeze c)
      === (elemsU v, List.map (const a) (elemsU v))

-- ---------------------------------------------------------------------- --
-- Growing

prop_grow :: V -> NonNegative Int -> Property
prop_grow v (NonNegative n) =
    withThawed v (\mv -> do
      g <- NEMV.grow mv n
      as <- freezeList (MV.take (NEMV.length mv) (_nemUvec g))
      pure (NEMV.length g, as))
      === (List.length (elemsU v) + n, elemsU v)

prop_grow_negative :: V -> Positive Int -> Property
prop_grow_negative v (Positive n) =
    throwsIO (thawIO v >>= \mv -> NEMV.grow mv (negate n))

prop_unsafeGrow :: V -> NonNegative Int -> Property
prop_unsafeGrow v (NonNegative n) =
    withThawed v (\mv -> do
      g <- NEMV.unsafeGrow mv n
      as <- freezeList (MV.take (NEMV.length mv) (_nemUvec g))
      pure (NEMV.length g, as))
      === (List.length (elemsU v) + n, elemsU v)

-- ---------------------------------------------------------------------- --
-- Accessing individual elements

prop_read :: V -> Property
prop_read v = forAll (genIndexU v) $ \i ->
    Just (withThawed v (`NEMV.read` i)) === lookupList i (elemsU v)

prop_read_out_of_bounds :: V -> Property
prop_read_out_of_bounds v = forAll (genOutOfBoundsU v) $ \i ->
    throwsIO (thawIO v >>= (`NEMV.read` i))

prop_write :: V -> Int -> Property
prop_write v a = forAll (genIndexU v) $ \i ->
    mutated v (\mv -> NEMV.write mv i a) === adjustAt i (const a) (elemsU v)

prop_write_out_of_bounds :: V -> Int -> Property
prop_write_out_of_bounds v a = forAll (genOutOfBoundsU v) $ \i ->
    throwsIO (thawIO v >>= \mv -> NEMV.write mv i a)

prop_modify :: Fun Int Int -> V -> Property
prop_modify (Fn f) v = forAll (genIndexU v) $ \i ->
    mutated v (\mv -> NEMV.modify mv f i) === adjustAt i f (elemsU v)

prop_swap :: V -> Property
prop_swap v = forAll ((,) <$> genIndexU v <*> genIndexU v) $ \(i, j) ->
    mutated v (\mv -> NEMV.swap mv i j) === swapAt i j (elemsU v)

prop_unsafeRead :: V -> Property
prop_unsafeRead v = forAll (genIndexU v) $ \i ->
    Just (withThawed v (`NEMV.unsafeRead` i)) === lookupList i (elemsU v)

prop_unsafeWrite :: V -> Int -> Property
prop_unsafeWrite v a = forAll (genIndexU v) $ \i ->
    mutated v (\mv -> NEMV.unsafeWrite mv i a) === adjustAt i (const a) (elemsU v)

prop_unsafeModify :: Fun Int Int -> V -> Property
prop_unsafeModify (Fn f) v = forAll (genIndexU v) $ \i ->
    mutated v (\mv -> NEMV.unsafeModify mv f i) === adjustAt i f (elemsU v)

prop_unsafeSwap :: V -> Property
prop_unsafeSwap v = forAll ((,) <$> genIndexU v <*> genIndexU v) $ \(i, j) ->
    mutated v (\mv -> NEMV.unsafeSwap mv i j) === swapAt i j (elemsU v)

-- ---------------------------------------------------------------------- --
-- Modifying vectors

-- Starting from sorted order, successive permutations enumerate every
-- distinct permutation in lexicographic order.
prop_nextPermutation :: Property
prop_nextPermutation =
    forAll (choose (1, 5)) $ \n ->
    forAll (vectorOf n (choose (0, 3))) $ \as ->
      let sorted = List.sort (as :: [Int])
      in withThawed (vecU sorted) enumerate
           === List.map NonEmpty.head
                 (NonEmpty.group (List.sort (List.permutations sorted)))
  where
    enumerate :: NonEmptySTUVector s Int -> ST s [[Int]]
    enumerate mv = do
      as <- freeze mv
      more <- NEMV.nextPermutation mv
      if more then (as :) <$> enumerate mv else pure [as]

-- The last permutation has no successor and is left unchanged.
prop_nextPermutation_last :: V -> Property
prop_nextPermutation_last v =
    withThawed (vecU descending) (\mv -> (,) <$> NEMV.nextPermutation mv <*> freeze mv)
      === (False, descending)
  where
    descending = List.sortBy (flip compare) (elemsU v)

-- ---------------------------------------------------------------------- --
-- Filling and copying

prop_set :: V -> Int -> Property
prop_set v a =
    mutated v (`NEMV.set` a) === List.map (const a) (elemsU v)

-- | Copy @v@ over a vector of the same length, returning the target and
-- source contents afterwards.
--
copied
    :: (forall s. NonEmptySTUVector s Int -> NonEmptySTUVector s Int -> ST s ())
    -> V
    -> [Int]
    -> ([Int], [Int])
copied cp v bs = runST $ do
    dst <- thaw (vecU bs)
    src <- thaw v
    cp dst src
    (,) <$> freeze dst <*> freeze src

prop_copy :: V -> Property
prop_copy v = forAll (vectorOf (List.length (elemsU v)) arbitrary) $ \bs ->
    copied NEMV.copy v bs === (elemsU v, elemsU v)

prop_copy_length_mismatch :: V -> Property
prop_copy_length_mismatch v = throwsIO $ do
    dst <- thawIO (vecU (0 : elemsU v))
    src <- thawIO v
    NEMV.copy dst src

prop_unsafeCopy :: V -> Property
prop_unsafeCopy v = forAll (vectorOf (List.length (elemsU v)) arbitrary) $ \bs ->
    copied NEMV.unsafeCopy v bs === (elemsU v, elemsU v)

-- | Move between the overlapping slices @[0, n - 1)@ and @[1, n)@ of one
-- vector, in both directions.
--
moved
    :: (forall s. NonEmptySTUVector s Int -> NonEmptySTUVector s Int -> ST s ())
    -> V
    -> ([Int], [Int])
moved mv v = (shift True, shift False)
  where
    n = List.length (elemsU v)

    shift left = withThawed v $ \(NonEmptyMUVector raw) -> do
      let lo = NonEmptyMUVector (MV.slice 0 (n - 1) raw)
          hi = NonEmptyMUVector (MV.slice 1 (n - 1) raw)
      if left then mv lo hi else mv hi lo
      freezeList raw

prop_move :: V -> Property
prop_move v = List.length as >= 2 ==>
    moved NEMV.move v
      === (List.drop 1 as ++ List.drop (n - 1) as, List.take 1 as ++ List.take (n - 1) as)
  where
    as = elemsU v
    n = List.length as

prop_unsafeMove :: V -> Property
prop_unsafeMove v = List.length as >= 2 ==>
    moved NEMV.unsafeMove v
      === (List.drop 1 as ++ List.drop (n - 1) as, List.take 1 as ++ List.take (n - 1) as)
  where
    as = elemsU v
    n = List.length as
