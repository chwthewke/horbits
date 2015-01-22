{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE RecordWildCards #-}

module Horbits.OrbitClassicalTest where

import           Control.Applicative
import           Control.Lens                         hiding (has, (*~), _1, _2)
import           Control.Rematch
import           Horbits.Body
import           Horbits.DimLin
import           Horbits.Orbit
import           Horbits.OrbitEq
import           Horbits.OrbitGen
import           Horbits.OrbitSample
import           Horbits.Rematch
import           Numeric.Units.Dimensional.TF.Prelude hiding (mod)
import           Prelude                              hiding (cos, mod, negate,
                                                       pi, sin, sqrt, (*), (+),
                                                       (-), (/))
import           Test.Framework                       hiding (sample)

genOrbits :: Gen Orbit
genOrbits = stdRandomOrbit $ getBody Kerbin


runApprox :: (a -> Dimensionless Double -> Bool) -> a -> Bool
runApprox f = flip f $ 1e-12 *~ one

isEquatorial :: OrbitSample -> Dimensionless Double -> Bool
isEquatorial OrbitSample{..} = incl `mod` pi =~ (_0 :: Dimensionless Double)

isCircular :: OrbitSample -> Dimensionless Double -> Bool
isCircular OrbitSample{..} = e =~ (_0 :: Dimensionless Double)


matchRaan :: OrbitSample -> Dimensionless Double -> (String, Matcher Orbit)
matchRaan sample@OrbitSample{..} = do
  eq <- isEquatorial sample
  let expectedRaan = if eq then _0 else raan
  has ("RAAN", _rightAscensionOfAscendingNode) <$> closeTo expectedRaan

matchArgPe :: OrbitSample -> Dimensionless Double -> (String, Matcher Orbit)
matchArgPe sample@OrbitSample{..} = do
  circ <- isCircular sample
  let expectedArgPe = if circ then _0 else arg
  has ("ARGPE", _argumentOfPeriapsis) <$> closeTo expectedArgPe

matchClassicalElements :: OrbitSample -> Dimensionless Double -> Matcher Orbit
matchClassicalElements sample@OrbitSample{..} = allOf' <$> sequence
    [has ("SMA", _semiMajorAxis) <$> relativelyCloseTo sma,
     has ("ECC", _eccentricity) <$> closeTo e,
     has ("INC", _inclination) <$> closeTo incl,
     matchRaan sample,
     matchArgPe sample
    ]

classicalElementsApproximatelyEqualExpected :: Double -> OrbitSample -> Bool
classicalElementsApproximatelyEqualExpected tolerance sample =
  match (matchClassicalElements sample (tolerance *~ one)) (orbit sample)

sampleClassicalElementsApproximatelyEqualExpected :: Double -> OrbitSample -> Property
sampleClassicalElementsApproximatelyEqualExpected tolerance sample =
  matcherProperty (matchClassicalElements sample $ tolerance *~ one) (orbit sample)


prop_sampleOrbitsShouldHaveExpectedClassicalElements :: Property
prop_sampleOrbitsShouldHaveExpectedClassicalElements =
  forAll genSampleOrbits $ sampleClassicalElementsApproximatelyEqualExpected 1e-12

adaptToleranceFor :: Orbit -> Dimensionless Double -> Dimensionless Double
adaptToleranceFor orbit = (/ (_1 - _eccentricity orbit))

checkAngularMomentumAtApside :: (Orbit -> Length Double) -> Orbit -> Dimensionless Double -> Bool
checkAngularMomentumAtApside apside orbit =
  (height * sqrt (mu * (_2 / height - _1 / sma)) =~~ norm (orbit ^. angularMomentum)) . adaptToleranceFor orbit
  where height = (orbit ^. orbitBody . bodyRadius) + apside orbit
        sma = _semiMajorAxis orbit
        mu = orbit ^. orbitBody ^. gravitationalParam


prop_AngularMomentumAtPe :: Property
prop_AngularMomentumAtPe =
  forAll genOrbits $ runApprox (checkAngularMomentumAtApside _periapsis)

prop_AngularMomentumAtAp :: Property
prop_AngularMomentumAtAp =
  forAll genOrbits $ runApprox (checkAngularMomentumAtApside _apoapsis)

checkApPlusPePlusDiameterIsTwiceSma :: Orbit -> Dimensionless Double -> Bool
checkApPlusPePlusDiameterIsTwiceSma orbit =
  _2 * orbit ^. orbitBody . bodyRadius + _apoapsis orbit + _periapsis orbit =~~ _2 * _semiMajorAxis orbit

prop_ApPeSma :: Property
prop_ApPeSma = forAll genOrbits $ runApprox checkApPlusPePlusDiameterIsTwiceSma

checkApPeEccentricity :: Orbit -> Dimensionless Double -> Bool
checkApPeEccentricity orbit =
  ((radius + _apoapsis orbit) * (_1 - _eccentricity orbit) =~~
    (radius + _periapsis orbit) * (_1 + _eccentricity orbit)) .
    adaptToleranceFor orbit
  where radius = orbit ^. orbitBody . bodyRadius

prop_ApPeEccentricity :: Property
prop_ApPeEccentricity = forAll genOrbits $ runApprox checkApPeEccentricity

checkHzInclination :: Orbit -> Dimensionless Double -> Bool
checkHzInclination orbit =
  orbit ^. angularMomentum . _z =~~ cos (_inclination orbit) * norm (orbit ^. angularMomentum)

prop_HzInclination :: Property
prop_HzInclination = forAll genOrbits $ runApprox checkHzInclination

checkHxyRaan :: Orbit -> Dimensionless Double -> Bool
checkHxyRaan orbit =
  cos raan' * orbit ^. angularMomentum . _x =~~ negate (sin raan' * orbit ^. angularMomentum . _y)
  where raan' = _rightAscensionOfAscendingNode orbit

prop_HxyRaan :: Property
prop_HxyRaan = forAll genOrbits $ runApprox checkHxyRaan

checkEzInclArgPe :: Orbit -> Dimensionless Double -> Bool
checkEzInclArgPe orbit =
  _eccentricityVector orbit ^. _z =~~ sin (_inclination orbit) * sin (_argumentOfPeriapsis orbit) * _eccentricity orbit

prop_EzInclArgPe :: Property
prop_EzInclArgPe = forAll genOrbits $ runApprox checkEzInclArgPe
