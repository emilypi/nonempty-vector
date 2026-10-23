-- |
-- Module       : Main (tests)
-- Copyright 	: (c) 2019-2026 Emily Pillmore
-- License	: BSD
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : TypeFamilies
--
module Main
( main
) where


import qualified Test.Data.Vector.NonEmpty.Accessors as Accessors
import qualified Test.Data.Vector.NonEmpty.Construction as Construction
import qualified Test.Data.Vector.NonEmpty.Conversion as Conversion
import qualified Test.Data.Vector.NonEmpty.Elementwise as Elementwise
import qualified Test.Data.Vector.NonEmpty.Folds as Folds
import qualified Test.Data.Vector.NonEmpty.Grouping as Grouping
import qualified Test.Data.Vector.NonEmpty.Instances as Instances
import qualified Test.Data.Vector.NonEmpty.Modification as Modification
import qualified Test.Data.Vector.NonEmpty.Mutable as Mutable
import qualified Test.Data.Vector.NonEmpty.Predicates as Predicates
import qualified Test.Data.Vector.NonEmpty.Scans as Scans

import Test.Tasty


main :: IO ()
main = defaultMain $ testGroup "NonEmptyVector tests"
    [ Accessors.tests
    , Construction.tests
    , Conversion.tests
    , Modification.tests
    , Elementwise.tests
    , Predicates.tests
    , Grouping.tests
    , Folds.tests
    , Scans.tests
    , Instances.tests
    , Mutable.tests
    ]
