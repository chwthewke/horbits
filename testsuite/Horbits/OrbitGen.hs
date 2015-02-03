{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE TypeFamilies #-}

module Horbits.OrbitGen where

import           Control.Lens                 ((^.))
import           Data.Maybe                   (fromMaybe)
import           Horbits.Body
import           Horbits.DimLin
import           Horbits.Orbit
import           Horbits.Types
import           Linear.Metric                (Metric)
import           Numeric.Units.Dimensional.TF hiding ((^/))
import           Prelude                      hiding (cos, pi, sin, sqrt, (*), (+), (-), (/))
import           System.Random
import           Test.Framework


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
        e0 <- sphericalV3 (_0, _1)
        eN <- chooseQuantity (loE, hiE) `suchThat` (< _1)
        let e = e0 `mkOrth` h `withNorm` eN
        return $ Orbit body h e (MeanAnomalyAtEpoch _0)

stdRandomOrbit :: BodyId -> Gen Orbit
stdRandomOrbit body = randomOrbit body (loH, hiH) (_0, _1)
  where loH = sqrt (body ^. fromBodyId . bodyGravitationalParam * minAlt)
        hiH = sqrt (_2 * body ^. fromBodyId . bodyGravitationalParam * minAlt)
        minAlt = body  ^. fromBodyId . bodyRadius + fromMaybe _0 (body ^. fromBodyId . bodyAtmosphereHeight)

orbitHasOrthogonalHAndE :: Orbit -> Bool
orbitHasOrthogonalHAndE orbit = e `dot` h < 1e-12 *. (norm e * norm h)
  where e = orbit ^. eccentricityVector
        h = orbit ^. angularMomentum

prop_generatedOrbitsHaveOrthogonalHAndE :: Property
prop_generatedOrbitsHaveOrthogonalHAndE =
  forAll (stdRandomOrbit Kerbin) orbitHasOrthogonalHAndE

orbitIsElliptical :: Orbit -> Bool
orbitIsElliptical orbit = norm (orbit ^. eccentricityVector) < _1

prop_generatedOrbitsAreElliptical :: Property
prop_generatedOrbitsAreElliptical =
  forAll (stdRandomOrbit Kerbin) orbitIsElliptical
