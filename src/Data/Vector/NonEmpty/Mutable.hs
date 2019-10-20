{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module      : Data.Vector.NonEmpty.Mutable
-- Copyright   : (c) 2019 Emily Pillmore
-- License     : BSD-style
--
-- Maintainer  : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability   : experimental
-- Portability : non-portable
--
-- Non-empty mutable boxed vectors.
--
module Data.Vector.NonEmpty.Mutable
( -- * Mutable boxed vectors
  NonEmptyMVector(..)
, NonEmptyIOVector
, NonEmptySTVector

  -- * Accessors
  -- ** Length information
, length

  -- ** Extracting subvectors
, slice, init, tail, take, drop, splitAt
, unsafeSlice, unsafeTake, unsafeDrop

  -- ** Overlapping
, overlaps

  -- * Construction

  -- ** Initialisation
, new, unsafeNew, replicate, replicateM, clone

  -- ** Growing
, grow, unsafeGrow

  -- ** Restricting memory usage
, clear

  -- * Accessing individual elements
, read, write, modify, swap
, unsafeRead, unsafeWrite, unsafeModify, unsafeSwap

  -- * Modifying vectors
, nextPermutation

  -- ** Filling and copying
, set, copy, move, unsafeCopy, unsafeMove
) where


import Prelude (Bool, Int, Ord, (.))

import Control.Monad.Primitive

import Data.Functor
import Data.Maybe (Maybe(..))
import Data.Typeable (Typeable)
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as M


-- | 'NonEmptyMVector' is a thin wrapper around 'MVector' that
-- witnesses an API requiring non-empty construction,
-- initialization, and generation of non-empty vectors by design.
--
-- A newtype wrapper was chosen so that no new pointer indirection
-- is introduced when working with 'MVector's, and all performance
-- characteristics inherited from the 'MVector' API still apply.
--
newtype NonEmptyMVector s a = NonEmptyMVector
    { _nemVec :: MVector s a }
    deriving (Typeable)

type NonEmptyIOVector = NonEmptyMVector RealWorld
type NonEmptySTVector = NonEmptyMVector

-- ---------------------------------------------------------------------- --
-- Length information

-- | Length of the mutable vector.
length :: NonEmptyMVector s a -> Int
length = M.length . _nemVec
{-# INLINE length #-}

-- ---------------------------------------------------------------------- --
-- Extracting subvectors

-- | Yield a part of the mutable vector without copying it.
slice :: Int -> Int -> NonEmptyMVector s a -> MVector s a
slice n m = M.slice n m . _nemVec
{-# INLINE slice #-}

take :: Int -> NonEmptyMVector s a -> MVector s a
take n = M.take n . _nemVec
{-# INLINE take #-}

drop :: Int -> NonEmptyMVector s a -> MVector s a
drop n = M.drop n . _nemVec
{-# INLINE drop #-}

splitAt :: Int -> NonEmptyMVector s a -> (MVector s a, MVector s a)
splitAt n = M.splitAt n . _nemVec
{-# INLINE splitAt #-}

init :: NonEmptyMVector s a -> MVector s a
init = M.unsafeInit . _nemVec
{-# INLINE init #-}

tail :: NonEmptyMVector s a -> MVector s a
tail = M.unsafeTail . _nemVec
{-# INLINE tail #-}

-- | Yield a part of the mutable vector without copying it. No bounds checks
-- are performed.
--
unsafeSlice
    :: Int
      -- ^ starting index
    -> Int
      -- ^ length of the slice
    -> NonEmptyMVector s a
    -> MVector s a
unsafeSlice n m = M.unsafeSlice n m . _nemVec
{-# INLINE unsafeSlice #-}

unsafeTake :: Int -> NonEmptyMVector s a -> MVector s a
unsafeTake n = M.unsafeTake n . _nemVec
{-# INLINE unsafeTake #-}

unsafeDrop :: Int -> NonEmptyMVector s a -> MVector s a
unsafeDrop n = M.unsafeDrop n . _nemVec
{-# INLINE unsafeDrop #-}

-- ---------------------------------------------------------------------- --
-- Overlapping

-- | Check whether two vectors overlap.
--
overlaps :: NonEmptyMVector s a -> NonEmptyMVector s a -> Bool
overlaps (NonEmptyMVector v) (NonEmptyMVector u) = M.overlaps v u
{-# INLINE overlaps #-}

-- ---------------------------------------------------------------------- --
-- Initialisation

fromMVector :: MVector s a -> Maybe (NonEmptyMVector s a)
fromMVector v = if M.null v then Nothing else Just (NonEmptyMVector v)

-- | Create a mutable vector of the given length.
--
new
    :: PrimMonad m
    => Int
    -> m (Maybe (NonEmptyMVector (PrimState m) a))
new = fmap fromMVector . M.new
{-# INLINE new #-}

-- | Create a mutable vector of the given length. The memory is not initialized.
--
unsafeNew
    :: PrimMonad m
    => Int
    -> m (Maybe (NonEmptyMVector (PrimState m) a))
unsafeNew = fmap fromMVector . M.unsafeNew
{-# INLINE unsafeNew #-}

-- | Create a mutable vector of the given length (0 if the length is negative)
-- and fill it with an initial value.
--
replicate
    :: PrimMonad m
    => Int
    -> a
    -> m (Maybe (NonEmptyMVector (PrimState m) a))
replicate n a = fmap fromMVector (M.replicate n a)
{-# INLINE replicate #-}

-- | Create a mutable vector of the given length (0 if the length is negative)
-- and fill it with values produced by repeatedly executing the monadic action.
--
replicateM
    :: PrimMonad m
    => Int
    -> m a
    -> m (Maybe (NonEmptyMVector (PrimState m) a))
replicateM  n a = fmap fromMVector (M.replicateM n a)
{-# INLINE replicateM #-}

-- | Create a copy of a mutable vector.
--
clone
    :: PrimMonad m
    => NonEmptyMVector (PrimState m) a
    -> m (NonEmptyMVector (PrimState m) a)
clone (NonEmptyMVector v) = fmap NonEmptyMVector (M.clone v)
{-# INLINE clone #-}

-- ---------------------------------------------------------------------- --
-- Growing

-- | Grow a vector by the given number of elements. The number must be
-- positive.
--
grow
    :: PrimMonad m
    => NonEmptyMVector (PrimState m) a
    -> Int
    -> m (NonEmptyMVector (PrimState m) a)
grow (NonEmptyMVector v) n = fmap NonEmptyMVector (M.grow v n)
{-# INLINE grow #-}

-- | Grow a vector by the given number of elements. The number must be
-- positive but this is not checked.
unsafeGrow
    :: PrimMonad m
    => NonEmptyMVector (PrimState m) a
    -> Int
    -> m (NonEmptyMVector (PrimState m) a)
unsafeGrow (NonEmptyMVector v) n = fmap NonEmptyMVector (M.unsafeGrow v n)
{-# INLINE unsafeGrow #-}

-- ---------------------------------------------------------------------- --
-- Restricting memory usage

-- | Reset all elements of the vector to some undefined value, clearing all
-- references to external objects. This is usually a noop for unboxed vectors.
clear :: PrimMonad m => NonEmptyMVector (PrimState m) a -> m ()
clear = M.clear . _nemVec
{-# INLINE clear #-}

-- ---------------------------------------------------------------------- --
-- Accessing individual elements

-- | Yield the element at the given position.
--
read
    :: PrimMonad m
    => NonEmptyMVector (PrimState m) a
    -> Int
    -> m a
read (NonEmptyMVector v) n = M.read v n
{-# INLINE read #-}

-- | Replace the element at the given position.
--
write
    :: PrimMonad m
    => NonEmptyMVector (PrimState m) a
    -> Int
    -> a
    -> m ()
write (NonEmptyMVector v) n a = M.write v n a
{-# INLINE write #-}

-- | Modify the element at the given position.
--
modify
    :: PrimMonad m
    => NonEmptyMVector (PrimState m) a
    -> (a -> a)
    -> Int
    -> m ()
modify (NonEmptyMVector v) f n = M.modify v f n
{-# INLINE modify #-}

-- | Swap the elements at the given positions.
--
swap
    :: PrimMonad m
    => NonEmptyMVector (PrimState m) a
    -> Int
    -> Int
    -> m ()
swap (NonEmptyMVector v) n m = M.swap v n m
{-# INLINE swap #-}

-- | Yield the element at the given position. No bounds checks are performed.
--
unsafeRead
    :: PrimMonad m
    => NonEmptyMVector (PrimState m) a
    -> Int
    -> m a
unsafeRead (NonEmptyMVector v) n = M.unsafeRead v n
{-# INLINE unsafeRead #-}

-- | Replace the element at the given position. No bounds checks are performed.
--
unsafeWrite
    :: PrimMonad m
    => NonEmptyMVector (PrimState m) a
    -> Int
    -> a
    -> m ()
unsafeWrite (NonEmptyMVector v) n a = M.unsafeWrite v n a
{-# INLINE unsafeWrite #-}

-- | Modify the element at the given position. No bounds checks are performed.
--
unsafeModify
    :: PrimMonad m
    => NonEmptyMVector (PrimState m) a
    -> (a -> a)
    -> Int
    -> m ()
unsafeModify (NonEmptyMVector v) f n = M.unsafeModify v f n
{-# INLINE unsafeModify #-}

-- | Swap the elements at the given positions. No bounds checks are performed.
--
unsafeSwap :: PrimMonad m => NonEmptyMVector (PrimState m) a -> Int -> Int -> m ()
unsafeSwap (NonEmptyMVector v) n m = M.unsafeSwap v n m
{-# INLINE unsafeSwap #-}

-- ---------------------------------------------------------------------- --
-- Filling and copying

-- | Set all elements of the vector to the given value.
--
set :: PrimMonad m => NonEmptyMVector (PrimState m) a -> a -> m ()
set (NonEmptyMVector v) a = M.set v a
{-# INLINE set #-}

-- | Copy a vector. The two vectors must have the same length and may not
-- overlap.
--
copy
    :: PrimMonad m
    => NonEmptyMVector (PrimState m) a
    -> NonEmptyMVector (PrimState m) a
    -> m ()
copy (NonEmptyMVector v) (NonEmptyMVector v') = M.copy v v'
{-# INLINE copy #-}

-- | Copy a vector. The two vectors must have the same length and may not
-- overlap. This is not checked.
--
unsafeCopy
    :: PrimMonad m
    => NonEmptyMVector (PrimState m) a
      -- ^ target
    -> NonEmptyMVector (PrimState m) a
      -- ^ source
    -> m ()
unsafeCopy (NonEmptyMVector v) (NonEmptyMVector v') = M.unsafeCopy v v'
{-# INLINE unsafeCopy #-}

-- | Move the contents of a vector. The two vectors must have the same
-- length.
--
-- If the vectors do not overlap, then this is equivalent to 'copy'.
-- Otherwise, the copying is performed as if the source vector were
-- copied to a temporary vector and then the temporary vector was copied
-- to the target vector.
--
move
    :: PrimMonad m
    => NonEmptyMVector (PrimState m) a
    -> NonEmptyMVector (PrimState m) a -> m ()
move (NonEmptyMVector v) (NonEmptyMVector v') = M.move v v'
{-# INLINE move #-}

-- | Move the contents of a vector. The two vectors must have the same
-- length, but this is not checked.
--
-- If the vectors do not overlap, then this is equivalent to 'unsafeCopy'.
-- Otherwise, the copying is performed as if the source vector were
-- copied to a temporary vector and then the temporary vector was copied
-- to the target vector.
--
unsafeMove
    :: PrimMonad m
    => NonEmptyMVector (PrimState m) a
      -- ^ target
    -> NonEmptyMVector (PrimState m) a
      -- ^ source
    -> m ()
unsafeMove (NonEmptyMVector v) (NonEmptyMVector v') = M.unsafeMove v v'
{-# INLINE unsafeMove #-}

-- | Compute the next (lexicographically) permutation of given vector in-place.
--   Returns False when input is the last permtuation
--
nextPermutation
    :: (PrimMonad m,Ord e)
    => NonEmptyMVector (PrimState m) e
    -> m Bool
nextPermutation = M.nextPermutation . _nemVec
{-# INLINE nextPermutation #-}
