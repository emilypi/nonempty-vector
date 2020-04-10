{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Vector.NonEmpty.Internal
( NonEmptyVector(..)
, NonEmptyMVector(..)
) where


import Control.DeepSeq
import Control.Monad.Zip (MonadZip)

import Data.Data
import Data.Functor.Classes (Eq1, Ord1, Show1)
import Data.Typeable (Typeable)
import qualified Data.Vector as V

newtype NonEmptyVector a = NonEmptyVector
    { _neVec :: V.Vector a
    } deriving
      ( Eq, Ord
      , Eq1, Ord1, Show1
      , Data, Typeable, NFData
      , Functor, Applicative, Monad
      , MonadZip
      , Semigroup
      )

newtype NonEmptyMVector s a = NonEmptyMVector
    { _nemVec :: V.MVector s a }
    deriving (Typeable)
