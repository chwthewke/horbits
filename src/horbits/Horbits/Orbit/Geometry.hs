{-# LANGUAGE NoImplicitPrelude #-}

module Horbits.Orbit.Geometry(orbitSemiAxes, orbitCenter, orbitSemiMajorAxisVector, orbitSemiMinorAxisVector) where

import           Control.Lens                hiding ((*~), _1, _2)

import           Horbits.Dimensional.Prelude
import           Horbits.Orbit.Class
import           Horbits.Orbit.Properties

orbitCenter :: OrbitClass t => Getter t (Length (V3 Double))
orbitCenter = to $ do
    e <- view orbitEccentricity
    smaa <- view orbitSemiMajorAxisVector
    return $ negate $ e *^ smaa

orbitSemiMajorAxisVector :: OrbitClass t => Getter t (Length (V3 Double))
orbitSemiMajorAxisVector = to $ do
    smaa <- view orbitSemiMajorAxis
    (ua, _) <- orbitSemiAxes
    return $ smaa *^ ua

orbitSemiMinorAxisVector :: OrbitClass t => Getter t (Length (V3 Double))
orbitSemiMinorAxisVector = to $ do
    smia <- view orbitSemiMinorAxis
    (_, ub) <- orbitSemiAxes
    return $ smia *^ ub

orbitSemiAxes :: OrbitClass t => t -> (Dimensionless (V3 Double), Dimensionless (V3 Double))
orbitSemiAxes orbit = if nearZero (orbit ^. orbitEccentricity)
                 then circularSemiAxes orbit
                 else eccentricSemiAxes orbit

eccentricSemiAxes :: OrbitClass t => t -> (Dimensionless (V3 Double), Dimensionless (V3 Double))
eccentricSemiAxes = do
    e <- view orbitEccentricityVector
    h <- view orbitAngularMomentumVector
    let a = normalize e
    let b = normalize $ h `cross` a
    return (a, b)

-- TODO tests would be great
circularSemiAxes :: OrbitClass t => t -> (Dimensionless (V3 Double), Dimensionless (V3 Double))
circularSemiAxes = do
    h <- view orbitAngularMomentumVector
    raan <- view orbitRightAscensionOfAscendingNode
    argp <- view orbitArgumentOfPeriapsis
    let c = v3 (cos raan) (sin raan) _0
    let a = rotate (axisAngle h (negate argp)) c
    let b = rotate (axisAngle h (pi / _2)) a
    return (a, b)
