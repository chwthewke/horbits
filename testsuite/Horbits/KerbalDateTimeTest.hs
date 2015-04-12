{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE Rank2Types                #-}

module Horbits.KerbalDateTimeTest where

import           Control.Applicative
import           Control.Lens                hiding (elements, (*~))
import qualified Prelude
import           Test.Framework

import           Horbits.Dimensional.Prelude
import           Horbits.KerbalDateTime
import           Horbits.OrbitEq
import           Horbits.Rematch

(|+|) :: Num a => a -> a -> a
(|+|) = (Prelude.+)

(|*|) :: Num a => a -> a -> a
(|*|) = (Prelude.*)

genKerbalTime :: Gen KerbalTime
genKerbalTime = do
    aDuration <- arbitrary
    return $ (getNonNegative aDuration *~ second) ^. from duration

data ComponentWeight = ComponentWeight String (Lens' KerbalTime Integer) Integer

integralComponentWeights :: [ComponentWeight]
integralComponentWeights = [ ComponentWeight "s" seconds 1
                           , ComponentWeight "m" minutes 60
                           , ComponentWeight "h" hours $ 60 |*| 60
                           , ComponentWeight "d" days $ 6 |*| 60 |*| 60
                           , ComponentWeight "y" years $ 426 |*| 6 |*| 60 |*| 60 ]

instance Show ComponentWeight where
    show (ComponentWeight n _ _ ) = n

tolerance :: Dimensionless Double
tolerance = 1e-12 *~ one

checkModifyingIntegralComponentVariesDurationAccordingToWeight ::
    KerbalTime -> ComponentWeight -> Integer -> Property
checkModifyingIntegralComponentVariesDurationAccordingToWeight t (ComponentWeight _ l w) n =
    matcherProperty (relativelyCloseTo expected tolerance) actual
  where
    expected = t ^. duration + (fromIntegral (n |*| w) :: Double) *~ second
    actual = (t & l +~ n) ^. duration

checkModifyingFractionalComponentVariesDurationBySameAmount :: KerbalTime -> Double -> Property
checkModifyingFractionalComponentVariesDurationBySameAmount t d =
    matcherProperty (relativelyCloseTo expected tolerance) actual
  where
    expected = t ^. duration + (d *~ second)
    actual = (t & secondsFraction +~ d) ^. duration


prop_ModifyingFractionalComponentVariesDurationBySameAmount :: Property
prop_ModifyingFractionalComponentVariesDurationBySameAmount =
    forAll ((,) <$> genKerbalTime <*> arbitrary) $
        uncurry checkModifyingFractionalComponentVariesDurationBySameAmount

prop_ModifyingIntegralComponentVariesDurationBySameAmount :: Property
prop_ModifyingIntegralComponentVariesDurationBySameAmount =
    forAll ((,,) <$> genKerbalTime <*> elements integralComponentWeights <*> arbitrary) $
        uncurry3 checkModifyingIntegralComponentVariesDurationAccordingToWeight
  where uncurry3 f (a, b, c) = f a b c

