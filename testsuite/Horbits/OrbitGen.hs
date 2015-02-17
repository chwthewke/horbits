{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE TypeFamilies #-}

module Horbits.OrbitGen where

import           Control.Applicative
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
        stabilize v' = if norm v' < 1e-12 *. norm v then zero else v'

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
        return $ Orbit body (OrbitSpecificAngularMomentum h) (Eccentricity e) (MeanAnomalyAtEpoch _0)

stdRandomOrbit :: BodyId -> Gen Orbit
stdRandomOrbit body = randomOrbit body (loH, hiH) (_0, _1)
  where loH = sqrt (body ^. fromBodyId . bodyGravitationalParam . _Wrapped' * minAlt)
        hiH = sqrt (_2 * body ^. fromBodyId . bodyGravitationalParam . _Wrapped'  * minAlt)
        minAlt = body  ^. fromBodyId . bodyRadius . _Wrapped' +
                 fromMaybe _0 (view _Wrapped' <$> body ^. fromBodyId . bodyAtmosphereHeight)

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
  return $ classical bId (SemiMajorAxis sma)
                            (Eccentricity ecc)
                            (RightAscensionOfAscendingNode raan)
                            (Inclination incl)
                            (ArgumentOfPeriapsis argPe)
                            (MeanAnomalyAtEpoch maae)
  where body = getBody bId
        minR = body ^. bodyRadius . _Wrapped' + fromMaybe _0 (view _Wrapped' <$> body ^. bodyAtmosphereHeight)
        maxR = fromMaybe (1e12 *~ meter) (view _Wrapped' <$> body ^. bodySphereOfInfluence)


-- properties of generators

orbitHasOrthogonalHAndE :: Orbit -> Bool
orbitHasOrthogonalHAndE orbit = e `dot` h < 1e-12 *. (norm e * norm h)
  where e = orbit ^. eccentricityVector . _Wrapped'
        h = orbit ^. angularMomentum . _Wrapped'

prop_generatedOrbitsHaveOrthogonalHAndE :: Property
prop_generatedOrbitsHaveOrthogonalHAndE =
  forAll (stdRandomOrbit Kerbin) orbitHasOrthogonalHAndE

prop_capturedOrbitsHaveOrthogonalHAndE :: Property
prop_capturedOrbitsHaveOrthogonalHAndE =
  forAll (anyBody >>= capturedOrbit) orbitHasOrthogonalHAndE

orbitIsElliptical :: Orbit -> Bool
orbitIsElliptical orbit = norm (orbit ^. eccentricityVector . _Wrapped') < _1

prop_generatedOrbitsAreElliptical :: Property
prop_generatedOrbitsAreElliptical =
  forAll (stdRandomOrbit Kerbin) orbitIsElliptical

prop_capturedOrbitsAreElliptical :: Property
prop_capturedOrbitsAreElliptical =
  forAll (anyBody >>= capturedOrbit) orbitIsElliptical
