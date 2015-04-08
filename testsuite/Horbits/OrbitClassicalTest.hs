{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE Rank2Types      #-}
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


adaptToleranceFor :: OrbitClass t => t -> Dimensionless Double -> Dimensionless Double
adaptToleranceFor orbit = (/ (_1 - orbit ^. orbitEccentricity))

mkMatcherProperty :: OrbitClass t =>
                     (t -> Dimensionless Double -> Matcher (Quantity d a))
                     -> (t -> Quantity d a)
                     -> (t -> Dimensionless Double -> Dimensionless Double)
                     -> Dimensionless Double
                     -> t
                     -> Property
mkMatcherProperty expected actual adapt tol orbit =
    matcherProperty (expected orbit (adapt orbit tol)) (actual orbit)


isEquatorial :: OrbitSample -> Dimensionless Double -> Bool
isEquatorial OrbitSample{..} = incl `mod` pi =~ _0

isCircular :: OrbitSample -> Dimensionless Double -> Bool
isCircular OrbitSample{..} = e =~ _0


matchRaan :: OrbitClass t => OrbitSample -> Dimensionless Double -> (String, Matcher t)
matchRaan sample@OrbitSample{..} = do
    eq <- isEquatorial sample
    let expectedRaan = if eq then _0 else raan
    has ("RAAN", orbitRightAscensionOfAscendingNode) <$> closeTo expectedRaan

matchArgPe :: OrbitClass t => OrbitSample -> Dimensionless Double -> (String, Matcher t)
matchArgPe sample@OrbitSample{..} = do
    circ <- isCircular sample
    let expectedArgPe = if circ then _0 else arg
    has ("ARGPE", orbitArgumentOfPeriapsis) <$> closeTo expectedArgPe

matchClassicalElements :: OrbitClass t => OrbitSample -> Dimensionless Double -> Matcher t
matchClassicalElements sample@OrbitSample{..} = allOf' <$> sequence
    [has ("SMA", orbitSemiMajorAxis) <$> relativelyCloseTo sma,
     has ("ECC", orbitEccentricity) <$> closeTo e,
     has ("INC", orbitInclination) <$> closeTo incl,
     matchRaan sample,
     matchArgPe sample
    ]

sampleClassicalElementsApproximatelyEqualExpected :: Dimensionless Double -> OrbitSample -> Property
sampleClassicalElementsApproximatelyEqualExpected tol sample =
    matcherProperty (matchClassicalElements sample tol) (orbit sample)


prop_sampleOrbitsShouldHaveExpectedClassicalElements :: Property
prop_sampleOrbitsShouldHaveExpectedClassicalElements =
    forAll genSampleOrbits $ sampleClassicalElementsApproximatelyEqualExpected tolerance

checkAngularMomentumAtApside :: OrbitClass t => Getter t (Length Double) -> t -> Property
checkAngularMomentumAtApside apside = mkMatcherProperty
    (\orbit -> relativelyCloseTo (orbit ^. apside * sqrt (mu orbit * (_2 / orbit ^. apside - _1 / sma orbit))))
    (view orbitAngularMomentum)
    adaptToleranceFor tolerance
  where
    sma orbit = orbit ^. orbitSemiMajorAxis
    mu orbit = orbit ^. orbitMu


prop_AngularMomentumAtPe :: Property
prop_AngularMomentumAtPe =
    forAll genOrbits $ checkAngularMomentumAtApside orbitPeriapsis

prop_AngularMomentumAtAp :: Property
prop_AngularMomentumAtAp =
    forAll genOrbits $ checkAngularMomentumAtApside orbitApoapsis

checkApPlusPeIsTwiceSma :: OrbitClass t => Dimensionless Double -> t -> Property
checkApPlusPeIsTwiceSma = mkMatcherProperty
    (\orbit -> relativelyCloseTo (_2 *  orbit ^. orbitSemiMajorAxis))
    (\orbit -> orbit ^. orbitApoapsis + orbit ^. orbitPeriapsis)
    (const id)

prop_ApPeSma :: Property
prop_ApPeSma = forAll genOrbits $ checkApPlusPeIsTwiceSma tolerance

checkApsideAltitude :: OrbitClass t => Getter t (Length Double) -> Getter t (Length Double) -> t -> Property
checkApsideAltitude refApside apside = mkMatcherProperty
    (relativelyCloseTo . view refApside)
    (\orbit -> orbit ^. apside + orbit ^. orbitBody . bodyRadius)
    (const id) tolerance

prop_ApAltitude :: Property
prop_ApAltitude = forAll genOrbits $ checkApsideAltitude orbitApoapsis orbitApoapsisAltitude

prop_PeAltitude :: Property
prop_PeAltitude = forAll genOrbits $ checkApsideAltitude orbitPeriapsis orbitPeriapsisAltitude

checkApPeEccentricity :: OrbitClass t => Dimensionless Double -> t -> Property
checkApPeEccentricity = mkMatcherProperty
    (\orbit -> relativelyCloseTo $ (orbit ^. orbitPeriapsis) * (_1 + orbit ^. orbitEccentricity))
    (\orbit -> (orbit ^. orbitApoapsis) * (_1 - orbit ^. orbitEccentricity))
    adaptToleranceFor

prop_ApPeEccentricity :: Property
prop_ApPeEccentricity = forAll genOrbits $ checkApPeEccentricity tolerance

checkHzInclination :: OrbitClass t => Dimensionless Double -> t -> Property
checkHzInclination = mkMatcherProperty
    (\orbit -> relativelyCloseTo $ cos (orbit ^. orbitInclination ) * orbit ^. orbitAngularMomentum)
    (\orbit -> orbit ^. orbitAngularMomentumVector . _z)
    (const id)


prop_HzInclination :: Property
prop_HzInclination = forAll genOrbits $ checkHzInclination tolerance

checkHxyRaan :: OrbitClass t => Dimensionless Double -> t -> Property
checkHxyRaan = mkMatcherProperty
    (\orbit -> relativelyCloseTo $ cos (raan' orbit) * orbit ^. orbitAngularMomentumVector . _x)
    (\orbit -> negate (sin (raan' orbit) * orbit ^. orbitAngularMomentumVector . _y))
    (const id)
  where raan' orbit = orbit ^. orbitRightAscensionOfAscendingNode

prop_HxyRaan :: Property
prop_HxyRaan = forAll genOrbits $ checkHxyRaan tolerance

checkEzInclArgPe :: OrbitClass t => Dimensionless Double -> t -> Property
checkEzInclArgPe = mkMatcherProperty
    (\orbit -> relativelyCloseTo $ orbit ^. orbitEccentricityVector . _z)
    (\orbit -> sin (orbit ^. orbitInclination) *
        sin (orbit ^. orbitArgumentOfPeriapsis) *
        orbit ^. orbitEccentricity)
    (const id)

prop_EzInclArgPe :: Property
prop_EzInclArgPe = forAll genOrbits $ checkEzInclArgPe tolerance

checkOrbitFromClassicalElements :: Orbit -> Dimensionless Double -> Bool
checkOrbitFromClassicalElements orbit = orbit =~~ cOrbit
  where cOrbit = Orbit (orbit ^. orbitBodyId)
                       (orbit ^. orbitSemiMajorAxis)
                       (orbit ^. orbitEccentricity)
                       (orbit ^. orbitRightAscensionOfAscendingNode)
                       (orbit ^. orbitInclination )
                       (orbit ^. orbitArgumentOfPeriapsis)
                       (orbit ^. orbitMeanAnomalyAtEpoch)

prop_orbitFromClassicalElements :: Property
prop_orbitFromClassicalElements = forAll genOrbits $ flip checkOrbitFromClassicalElements tolerance
