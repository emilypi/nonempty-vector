-- |
-- Module       : Main (tests)
-- Copyright 	: (c) 2019-2026 Emily Pillmore, Koz Ross
-- License	: BSD
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : TypeFamilies
--
module Main
( main
) where


import qualified Test.Data.Vector.NonEmpty.Accessors as Accessors
import qualified Test.Data.Vector.NonEmpty.AccessorsU as AccessorsU
import qualified Test.Data.Vector.NonEmpty.Construction as Construction
import qualified Test.Data.Vector.NonEmpty.ConstructionU as ConstructionU
import qualified Test.Data.Vector.NonEmpty.Conversion as Conversion
import qualified Test.Data.Vector.NonEmpty.ConversionU as ConversionU
import qualified Test.Data.Vector.NonEmpty.Elementwise as Elementwise
import qualified Test.Data.Vector.NonEmpty.ElementwiseU as ElementwiseU
import qualified Test.Data.Vector.NonEmpty.Folds as Folds
import qualified Test.Data.Vector.NonEmpty.FoldsU as FoldsU
import qualified Test.Data.Vector.NonEmpty.Grouping as Grouping
import qualified Test.Data.Vector.NonEmpty.GroupingU as GroupingU
import qualified Test.Data.Vector.NonEmpty.Instances as Instances
import qualified Test.Data.Vector.NonEmpty.InstancesU as InstancesU
import qualified Test.Data.Vector.NonEmpty.Modification as Modification
import qualified Test.Data.Vector.NonEmpty.ModificationU as ModificationU
import qualified Test.Data.Vector.NonEmpty.Mutable as Mutable
import qualified Test.Data.Vector.NonEmpty.MutableU as MutableU
import qualified Test.Data.Vector.NonEmpty.Predicates as Predicates
import qualified Test.Data.Vector.NonEmpty.Scans as Scans

import Test.Tasty


main :: IO ()
main = defaultMain $ testGroup "NonEmptyVector tests"
    [ Accessors.tests
    , AccessorsU.tests
    , Construction.tests
    , ConstructionU.tests
    , Conversion.tests
    , ConversionU.tests
    , Modification.tests
    , ModificationU.tests
    , Elementwise.tests
    , ElementwiseU.tests
    , Predicates.tests
    , Grouping.tests
    , GroupingU.tests
    , Folds.tests
    , FoldsU.tests
    , Scans.tests
    , Instances.tests
    , InstancesU.tests
    , Mutable.tests
    , MutableU.tests
    ]
