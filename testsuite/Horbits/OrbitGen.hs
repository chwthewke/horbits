{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE TypeFamilies #-}

module Horbits.OrbitGen where

import           Control.Lens                         hiding ((*~), _1, _2)
import           Data.Maybe                           (fromMaybe)
import           Horbits.Body
import           Horbits.DimLin
import           Horbits.Orbit
import           Horbits.Types
import           Linear.Metric                        (Metric)
import           Numeric.Units.Dimensional.TF         (Dimensional (Dimensional))
import           Numeric.Units.Dimensional.TF.Prelude hiding (zero, (^/))
import           Prelude                              hiding (cos, pi, sin, sqrt, (*), (+), (-), (/))
import           System.Random
import           Test.Framework

anyBody :: Gen BodyId
anyBody = arbitraryBoundedEnum

chooseQuantity :: (Random a) => (Quantity d a, Quantity d a) -> Gen (Quantity d a)
chooseQuantity (Dimensional lo, Dimensional hi) = do
  val <- choose (lo, hi)
  return $ Dimensional val

mkOrth :: (DOne ~ Div d d, d' ~ Mul DOne d') =>
            Quantity d' (V3 Double) -> Quantity d (V3 Double) -> Quantity d' (V3 Double)
mkOrth v ref = stabilize $ if norm ref == _0 then v else refUnit `cross` v
  where refUnit = ref ^/ norm ref
        stabilize v' = if norm v' < 1e-12 *. norm v then zero else v' -- TODO dimNearZero

withNorm :: (d' ~ Mul (Div d' d) d, Metric f) => Quantity d (f Double) -> Quantity d' Double -> Quantity d' (f Double)
withNorm v n =
    if norm v == _0
      then zero
      else (n / norm v) *^ v

sphericalV3 :: (d ~ Mul DOne d) => (Quantity d Double, Quantity d Double) -> Gen (Quantity d (V3 Double))
sphericalV3 (lo, hi) = do
  n <- chooseQuantity (lo, hi)
  colat <- chooseQuantity (_0, pi)
  long <- chooseQuantity (_0, _2 * pi)
  return $ v3 (sin colat * cos long * n) (sin colat * sin long * n) (cos colat * n)

randomOrbit :: BodyId ->
                 (Quantity DSpecificAngularMomentum Double, Quantity DSpecificAngularMomentum Double) ->
                 (Dimensionless Double, Dimensionless Double) ->
                 Gen Orbit
randomOrbit body (loH, hiH) (loE, hiE) = do
        h <- sphericalV3 (loH, hiH)
        e0 <- sphericalV3 (_1, _1)
        eN <- chooseQuantity (loE, hiE) `suchThat` (< _1)
        let e = e0 `mkOrth` h `withNorm` eN
        return $ vectorOrbit body h e _0

stdRandomOrbit :: BodyId -> Gen Orbit
stdRandomOrbit body = randomOrbit body (loH, hiH) (_0, _1)
  where loH = sqrt (body ^. fromBodyId . bodyGravitationalParam * minAlt)
        hiH = sqrt (_2 * body ^. fromBodyId . bodyGravitationalParam  * minAlt)
        minAlt = body  ^. fromBodyId . bodyRadius +
                 fromMaybe _0 (body ^. fromBodyId . bodyAtmosphereHeight)

capturedOrbit :: BodyId -> Gen Orbit
capturedOrbit bId = do
  ap <- chooseQuantity (minR, maxR)
  pe <- chooseQuantity (minR, ap)
  let sma = (ap + pe) / _2
  let ecc = ap / sma - _1
  raan <- chooseQuantity (_0, tau)
  incl <- chooseQuantity (_0, pi)
  argPe <- chooseQuantity (_0, tau)
  maae <- chooseQuantity (_0, tau)
  return $ Orbit bId sma ecc raan incl argPe maae
  where body = getBody bId
        minR = body ^. bodyRadius + fromMaybe _0 (body ^. bodyAtmosphereHeight)
        maxR = fromMaybe (1e12 *~ meter) (body ^? bodySphereOfInfluence)


-- properties of generators

orbitHasOrthogonalHAndE :: Orbit -> Bool
orbitHasOrthogonalHAndE orbit = e `dot` h < 1e-12 *. (norm e * norm h)
  where e = orbit ^. orbitEccentricityVector
        h = orbit ^. orbitAngularMomentumVector

prop_generatedOrbitsHaveOrthogonalHAndE :: Property
prop_generatedOrbitsHaveOrthogonalHAndE =
  forAll (stdRandomOrbit Kerbin) orbitHasOrthogonalHAndE

prop_capturedOrbitsHaveOrthogonalHAndE :: Property
prop_capturedOrbitsHaveOrthogonalHAndE =
  forAll (anyBody >>= capturedOrbit) orbitHasOrthogonalHAndE

orbitIsElliptical :: Orbit -> Bool
orbitIsElliptical orbit = orbit ^. orbitEccentricity < _1

prop_generatedOrbitsAreElliptical :: Property
prop_generatedOrbitsAreElliptical =
  forAll (stdRandomOrbit Kerbin) orbitIsElliptical

prop_capturedOrbitsAreElliptical :: Property
prop_capturedOrbitsAreElliptical =
  forAll (anyBody >>= capturedOrbit) orbitIsElliptical
