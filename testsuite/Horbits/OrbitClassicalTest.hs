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
    has ("RAAN", rightAscensionOfAscendingNode . _Wrapped') <$> closeTo expectedRaan

matchArgPe :: OrbitSample -> Dimensionless Double -> (String, Matcher Orbit)
matchArgPe sample@OrbitSample{..} = do
    circ <- isCircular sample
    let expectedArgPe = if circ then _0 else arg
    has ("ARGPE", argumentOfPeriapsis . _Wrapped') <$> closeTo expectedArgPe

matchClassicalElements :: OrbitSample -> Dimensionless Double -> Matcher Orbit
matchClassicalElements sample@OrbitSample{..} = allOf' <$> sequence
    [has ("SMA", semiMajorAxis . _Wrapped') <$> relativelyCloseTo sma,
     has ("ECC", eccentricity . _Wrapped') <$> closeTo e,
     has ("INC", inclination . _Wrapped') <$> closeTo incl,
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
adaptToleranceFor orbit = (/ (_1 - orbit ^. eccentricity . _Wrapped'))

checkAngularMomentumAtApside :: Getting (Length Double) Orbit (Length Double) -> Orbit -> Dimensionless Double -> Bool
checkAngularMomentumAtApside apside orbit =
    (height * sqrt (mu * (_2 / height - _1 / sma)) =~~ norm (orbit ^. angularMomentum . _Wrapped'))
        . adaptToleranceFor orbit
  where
    height = (orbit ^. orbitBody . bodyRadius . _Wrapped') + orbit ^. apside
    sma = orbit ^. semiMajorAxis . _Wrapped'
    mu = orbit ^. orbitMu . _Wrapped'


prop_AngularMomentumAtPe :: Property
prop_AngularMomentumAtPe =
    forAll genOrbits $ runApprox (checkAngularMomentumAtApside $ periapsis . _Wrapped')

prop_AngularMomentumAtAp :: Property
prop_AngularMomentumAtAp =
    forAll genOrbits $ runApprox (checkAngularMomentumAtApside $ apoapsis . _Wrapped')

checkApPlusPePlusDiameterIsTwiceSma :: Orbit -> Dimensionless Double -> Bool
checkApPlusPePlusDiameterIsTwiceSma orbit =
    _2 * orbit ^. orbitBody . bodyRadius . _Wrapped' +
        orbit ^. apoapsis . _Wrapped' + orbit ^. periapsis . _Wrapped' =~~ _2 *  orbit ^. semiMajorAxis . _Wrapped'

prop_ApPeSma :: Property
prop_ApPeSma = forAll genOrbits $ runApprox checkApPlusPePlusDiameterIsTwiceSma

checkApPeEccentricity :: Orbit -> Dimensionless Double -> Bool
checkApPeEccentricity orbit =
    ((radius + orbit ^. apoapsis . _Wrapped') * (_1 - orbit ^. eccentricity . _Wrapped') =~~
        (radius + orbit ^. periapsis . _Wrapped') * (_1 + orbit ^. eccentricity . _Wrapped')) .
        adaptToleranceFor orbit
  where radius = orbit ^. orbitBody . bodyRadius . _Wrapped'

prop_ApPeEccentricity :: Property
prop_ApPeEccentricity = forAll genOrbits $ runApprox checkApPeEccentricity

checkHzInclination :: Orbit -> Dimensionless Double -> Bool
checkHzInclination orbit =
    orbit ^. angularMomentum . _Wrapped' . _z =~~
        cos (orbit ^. inclination . _Wrapped') * norm (orbit ^. angularMomentum . _Wrapped')

prop_HzInclination :: Property
prop_HzInclination = forAll genOrbits $ runApprox checkHzInclination

checkHxyRaan :: Orbit -> Dimensionless Double -> Bool
checkHxyRaan orbit =
    cos raan' * orbit ^. angularMomentum . _Wrapped' . _x =~~
        negate (sin raan' * orbit ^. angularMomentum . _Wrapped' . _y)
  where raan' = orbit ^. rightAscensionOfAscendingNode . _Wrapped'

prop_HxyRaan :: Property
prop_HxyRaan = forAll genOrbits $ runApprox checkHxyRaan

checkEzInclArgPe :: Orbit -> Dimensionless Double -> Bool
checkEzInclArgPe orbit =
    orbit ^. eccentricityVector . _Wrapped' . _z =~~
        sin (orbit ^. inclination . _Wrapped') *
        sin (orbit ^. argumentOfPeriapsis . _Wrapped') *
        orbit ^. eccentricity . _Wrapped'

prop_EzInclArgPe :: Property
prop_EzInclArgPe = forAll genOrbits $ runApprox checkEzInclArgPe

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
prop_orbitFromClassicalElements = forAll genOrbits $ runApprox checkOrbitFromClassicalElements
