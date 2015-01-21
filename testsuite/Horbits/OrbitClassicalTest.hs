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
import           Horbits.Types
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
isEquatorial OrbitSample{..} = incl `mod` pi =~ _0

isCircular :: OrbitSample -> Dimensionless Double -> Bool
isCircular OrbitSample{..} = e =~ _0


matchRaan :: OrbitSample -> Dimensionless Double -> (String, Matcher Orbit)
matchRaan sample@OrbitSample{..} = do
  eq <- isEquatorial sample
  let expectedRaan = if eq then _0 else raan
  has ("RAAN", view measure . _rightAscensionOfAscendingNode) <$> closeTo expectedRaan

matchArgPe :: OrbitSample -> Dimensionless Double -> (String, Matcher Orbit)
matchArgPe sample@OrbitSample{..} = do
  circ <- isCircular sample
  let expectedArgPe = if circ then _0 else arg
  has ("ARGPE", view measure . _argumentOfPeriapsis) <$> closeTo expectedArgPe

matchClassicalElements :: OrbitSample -> Dimensionless Double -> Matcher Orbit
matchClassicalElements sample@OrbitSample{..} = allOf' <$> sequence
    [has ("SMA", view measure . _semiMajorAxis) <$> relativelyCloseTo sma,
     has ("ECC", view measure . _eccentricity) <$> closeTo e,
     has ("INC", view measure . _inclination) <$> closeTo incl,
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
adaptToleranceFor orbit = (/ (_1 - _eccentricity orbit ^. measure))

checkAngularMomentumAtApside :: (Orbit -> Length Double) -> Orbit -> Dimensionless Double -> Bool
checkAngularMomentumAtApside apside orbit =
  (height * sqrt (mu * (_2 / height - _1 / sma)) =~~ norm (orbit ^. angularMomentum)) . adaptToleranceFor orbit
  where height = (orbit ^. orbitBody . bodyRadius) + apside orbit
        sma = _semiMajorAxis orbit ^. measure
        mu = orbit ^. orbitBody ^. gravitationalParam


prop_AngularMomentumAtPe :: Property
prop_AngularMomentumAtPe =
  forAll genOrbits $ runApprox (checkAngularMomentumAtApside (view measure . _periapsis))

prop_AngularMomentumAtAp :: Property
prop_AngularMomentumAtAp =
  forAll genOrbits $ runApprox (checkAngularMomentumAtApside (view measure . _apoapsis))

checkApPlusPePlusDiameterIsTwiceSma :: Orbit -> Dimensionless Double -> Bool
checkApPlusPePlusDiameterIsTwiceSma orbit =
  _2 * orbit ^. orbitBody . bodyRadius +
    _apoapsis orbit ^. measure + _periapsis orbit ^. measure =~~ _2 * _semiMajorAxis orbit ^. measure

prop_ApPeSma :: Property
prop_ApPeSma = forAll genOrbits $ runApprox checkApPlusPePlusDiameterIsTwiceSma

checkApPeEccentricity :: Orbit -> Dimensionless Double -> Bool
checkApPeEccentricity orbit =
  ((radius + _apoapsis orbit ^. measure) * (_1 - _eccentricity orbit ^. measure) =~~
    (radius + _periapsis orbit ^. measure) * (_1 + _eccentricity orbit ^. measure)) .
    adaptToleranceFor orbit
  where radius = orbit ^. orbitBody . bodyRadius

prop_ApPeEccentricity :: Property
prop_ApPeEccentricity = forAll genOrbits $ runApprox checkApPeEccentricity

checkHzInclination :: Orbit -> Dimensionless Double -> Bool
checkHzInclination orbit =
  orbit ^. angularMomentum . _z =~~ cos (_inclination orbit ^. measure) * norm (orbit ^. angularMomentum)

prop_HzInclination :: Property
prop_HzInclination = forAll genOrbits $ runApprox checkHzInclination

checkHxyRaan :: Orbit -> Dimensionless Double -> Bool
checkHxyRaan orbit =
  cos raan' * orbit ^. angularMomentum . _x =~~ negate (sin raan' * orbit ^. angularMomentum . _y)
  where raan' = _rightAscensionOfAscendingNode orbit ^. measure

prop_HxyRaan :: Property
prop_HxyRaan = forAll genOrbits $ runApprox checkHxyRaan

checkEzInclArgPe :: Orbit -> Dimensionless Double -> Bool
checkEzInclArgPe orbit =
  _eccentricityVector orbit ^. _z =~~
    sin (_inclination orbit ^. measure) *
    sin (_argumentOfPeriapsis orbit ^. measure) *
    _eccentricity orbit ^. measure

prop_EzInclArgPe :: Property
prop_EzInclArgPe = forAll genOrbits $ runApprox checkEzInclArgPe

checkOrbitFromClassicalElements :: Orbit -> Dimensionless Double -> Bool
checkOrbitFromClassicalElements orbit = orbit =~~ cOrbit
  where cOrbit = classical (orbit ^. orbitBody)
                           (_semiMajorAxis orbit)
                           (_eccentricity orbit)
                           (_rightAscensionOfAscendingNode orbit)
                           (_inclination orbit)
                           (_argumentOfPeriapsis orbit)
                           (_meanAnomalyAtEpoch orbit)

prop_orbitFromClassicalElements :: Property
prop_orbitFromClassicalElements = forAll genOrbits $ runApprox checkOrbitFromClassicalElements
