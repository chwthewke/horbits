{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Horbits.OrbitTest where

import           Horbits.OrbitEq
import           Horbits.OrbitSample
import           Numeric.Units.Dimensional.Prelude
import           Prelude                           ()
import           Test.Framework


prop_sampleOrbitsShouldHaveExpectedClassicalElements :: Property
prop_sampleOrbitsShouldHaveExpectedClassicalElements =
  forAll genSampleOrbits $ sampleClassicalElementsApproximatelyEqualExpected 1e-12
