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
import           Prelude                              hiding (cos, mod, negate, pi, sin, sqrt, (*), (+), (-), (/))
import           Test.Framework                       hiding (sample)

genOrbits :: Gen Orbit
genOrbits = anyBody >>= capturedOrbit

tolerance :: Dimensionless Double
tolerance = 1e-12 *~ one


adaptToleranceFor :: Orbit -> Dimensionless Double -> Dimensionless Double
adaptToleranceFor orbit = (/ (_1 - orbit ^. eccentricity))

mkMatcherProperty :: (Orbit -> Dimensionless Double -> Matcher (Quantity d a))
                     -> (Orbit -> Quantity d a)
                     -> (Orbit -> Dimensionless Double -> Dimensionless Double)
                     -> Dimensionless Double
                     -> Orbit
                     -> Property
mkMatcherProperty expected actual adapt tol orbit =
    matcherProperty (expected orbit (adapt orbit tol)) (actual orbit)


isEquatorial :: OrbitSample -> Dimensionless Double -> Bool
isEquatorial OrbitSample{..} = incl `mod` pi =~ _0

isCircular :: OrbitSample -> Dimensionless Double -> Bool
isCircular OrbitSample{..} = e =~ _0


matchRaan :: OrbitSample -> Dimensionless Double -> (String, Matcher Orbit)
matchRaan sample@OrbitSample{..} = do
    eq <- isEquatorial sample
    let expectedRaan = if eq then _0 else raan
    has ("RAAN", rightAscensionOfAscendingNode) <$> closeTo expectedRaan

matchArgPe :: OrbitSample -> Dimensionless Double -> (String, Matcher Orbit)
matchArgPe sample@OrbitSample{..} = do
    circ <- isCircular sample
    let expectedArgPe = if circ then _0 else arg
    has ("ARGPE", argumentOfPeriapsis) <$> closeTo expectedArgPe

matchClassicalElements :: OrbitSample -> Dimensionless Double -> Matcher Orbit
matchClassicalElements sample@OrbitSample{..} = allOf' <$> sequence
    [has ("SMA", semiMajorAxis) <$> relativelyCloseTo sma,
     has ("ECC", eccentricity) <$> closeTo e,
     has ("INC", inclination) <$> closeTo incl,
     matchRaan sample,
     matchArgPe sample
    ]

sampleClassicalElementsApproximatelyEqualExpected :: Dimensionless Double -> OrbitSample -> Property
sampleClassicalElementsApproximatelyEqualExpected tol sample =
    matcherProperty (matchClassicalElements sample tol) (orbit sample)


prop_sampleOrbitsShouldHaveExpectedClassicalElements :: Property
prop_sampleOrbitsShouldHaveExpectedClassicalElements =
    forAll genSampleOrbits $ sampleClassicalElementsApproximatelyEqualExpected tolerance

checkAngularMomentumAtApside :: Getting (Length Double) Orbit (Length Double)
                                -> Dimensionless Double
                                -> Orbit
                                -> Property
checkAngularMomentumAtApside apside = mkMatcherProperty
    (\orbit -> relativelyCloseTo (height apside orbit * sqrt (mu orbit * (_2 / height apside orbit - _1 / sma orbit))))
    (\orbit -> norm $ orbit ^. angularMomentum)
    adaptToleranceFor
  where
    height aps orbit = (orbit ^. orbitBody . bodyRadius) + orbit ^. aps
    sma orbit = orbit ^. semiMajorAxis
    mu orbit = orbit ^. orbitMu


prop_AngularMomentumAtPe :: Property
prop_AngularMomentumAtPe =
    forAll genOrbits $ checkAngularMomentumAtApside periapsis tolerance

prop_AngularMomentumAtAp :: Property
prop_AngularMomentumAtAp =
    forAll genOrbits $ checkAngularMomentumAtApside apoapsis tolerance

checkApPlusPePlusDiameterIsTwiceSma :: Dimensionless Double -> Orbit -> Property
checkApPlusPePlusDiameterIsTwiceSma = mkMatcherProperty
    (\orbit -> relativelyCloseTo (_2 *  orbit ^. semiMajorAxis))
    (\orbit -> _2 * orbit ^. orbitBody . bodyRadius
        + orbit ^. apoapsis + orbit ^. periapsis)
    (const id)

prop_ApPeSma :: Property
prop_ApPeSma = forAll genOrbits $ checkApPlusPePlusDiameterIsTwiceSma tolerance


checkApPeEccentricity :: Dimensionless Double -> Orbit -> Property
checkApPeEccentricity = mkMatcherProperty
    (\orbit -> relativelyCloseTo $ (radius orbit + orbit ^. periapsis) * (_1 + orbit ^. eccentricity))
    (\orbit -> (radius orbit + orbit ^. apoapsis) * (_1 - orbit ^. eccentricity))
    adaptToleranceFor
  where radius orbit = orbit ^. orbitBody . bodyRadius

prop_ApPeEccentricity :: Property
prop_ApPeEccentricity = forAll genOrbits $ checkApPeEccentricity tolerance

checkHzInclination :: Dimensionless Double -> Orbit -> Property
checkHzInclination = mkMatcherProperty
    (\orbit -> relativelyCloseTo $ cos (orbit ^. inclination ) * norm (orbit ^. angularMomentum))
    (\orbit -> orbit ^. angularMomentum . _z)
    (const id)


prop_HzInclination :: Property
prop_HzInclination = forAll genOrbits $ checkHzInclination tolerance

checkHxyRaan :: Dimensionless Double -> Orbit -> Property
checkHxyRaan = mkMatcherProperty
    (\orbit -> relativelyCloseTo $ cos (raan' orbit) * orbit ^. angularMomentum . _x)
    (\orbit -> negate (sin (raan' orbit) * orbit ^. angularMomentum . _y))
    (const id)
  where raan' orbit = orbit ^. rightAscensionOfAscendingNode

prop_HxyRaan :: Property
prop_HxyRaan = forAll genOrbits $ checkHxyRaan tolerance

checkEzInclArgPe :: Dimensionless Double -> Orbit -> Property
checkEzInclArgPe = mkMatcherProperty
    (\orbit -> relativelyCloseTo $ orbit ^. eccentricityVector . _z)
    (\orbit -> sin (orbit ^. inclination) *
        sin (orbit ^. argumentOfPeriapsis) *
        orbit ^. eccentricity)
    (const id)

prop_EzInclArgPe :: Property
prop_EzInclArgPe = forAll genOrbits $ checkEzInclArgPe tolerance

checkOrbitFromClassicalElements :: Orbit -> Dimensionless Double -> Bool
checkOrbitFromClassicalElements orbit = orbit =~~ cOrbit
  where cOrbit = classical (orbit ^. orbitBodyId)
                           (orbit ^. semiMajorAxis)
                           (orbit ^. eccentricity)
                           (orbit ^. rightAscensionOfAscendingNode)
                           (orbit ^. inclination )
                           (orbit ^. argumentOfPeriapsis)
                           (orbit ^. meanAnomalyAtEpoch)

prop_orbitFromClassicalElements :: Property
prop_orbitFromClassicalElements = forAll genOrbits $ flip checkOrbitFromClassicalElements tolerance
