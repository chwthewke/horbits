{-# LANGUAGE Rank2Types #-}


module Horbits.Orbit.Properties
    (orbitSemiLatusRectum, orbitApoapsis, orbitApoapsisAltitude, orbitPeriapsis, orbitPeriapsisAltitude,
    orbitSemiMinorAxis, orbitPeriod, orbitSpecificEnergy, orbitVelocity, bodySolarDay)
  where

import           Control.Applicative
import           Control.Lens                         hiding (_1, _2)
import           Control.Monad                        (mfilter)
import           Numeric.Units.Dimensional.TF.Prelude hiding (subtract)
import           Prelude                              hiding (negate, sqrt, subtract, (*), (+), (-), (/), (^))

import           Horbits.Body
import           Horbits.DimLin
import           Horbits.Orbit.Class
import           Horbits.Orbit.Data
import           Horbits.Orbit.Velocity

-- SLR
_semiLatusRectum :: OrbitClass t => t -> Length Double
_semiLatusRectum = do
    h <- view orbitAngularMomentumVector
    mu <- view orbitMu
    return $ quadrance h / mu -- TODO: ||h||^2 / mu (do not use vector)

orbitSemiLatusRectum :: OrbitClass t => Getter t (Length Double)
orbitSemiLatusRectum = to _semiLatusRectum


-- AP

_apoapsis :: OrbitClass t => t -> Length Double
_apoapsis = do
    p <- view orbitSemiLatusRectum
    e <- view orbitEccentricity
    return $ p / (_1 - e)

_setApoapsis :: OrbitClass t => t -> Length Double -> t
_setApoapsis orbit ap = orbit & orbitSemiMajorAxis %~ (+ dAp)
                                    & orbitEccentricity .~ e
  where
    dAp = ap - _apoapsis orbit
    e = ap / (orbit ^. orbitSemiMajorAxis + dAp) -_1

orbitApoapsis :: OrbitClass t => Lens' t (Length Double)
orbitApoapsis = lens _apoapsis _setApoapsis

orbitApoapsisAltitude :: OrbitClass t => Lens' t (Length Double)
orbitApoapsisAltitude = subtractingBodyRadius orbitApoapsis


-- PE

_periapsis :: OrbitClass t => t -> Length Double
_periapsis = do
    p <- view orbitSemiLatusRectum
    e <- view orbitEccentricity
    return $ p / (_1 + e)

_setPeriapsis :: OrbitClass t => t -> Length Double -> t
_setPeriapsis orbit pe = orbit & orbitSemiMajorAxis %~ (+ dPe)
                                     & orbitEccentricity .~ e
  where
    dPe = pe - _periapsis orbit
    e = _1 - pe / (orbit ^. orbitSemiMajorAxis + dPe)

orbitPeriapsis :: OrbitClass t => Lens' t (Length Double)
orbitPeriapsis = lens _periapsis _setPeriapsis

orbitPeriapsisAltitude :: OrbitClass t => Lens' t (Length Double)
orbitPeriapsisAltitude = subtractingBodyRadius orbitPeriapsis

--

subtractingBodyRadius :: OrbitClass t => Lens' t (Length Double) -> Lens' t (Length Double)
subtractingBodyRadius l f orb =
    l (fmap (+ r) . f . subtract r) orb
  where r = orb ^. orbitBody . bodyRadius

-- Semi-minor Axis

orbitSemiMinorAxis :: OrbitClass t => Getter t (Length Double)
orbitSemiMinorAxis = to $ do
    sma <- view orbitSemiMajorAxis
    p <- view orbitSemiLatusRectum
    return $ sqrt $ sma * p

-- Period

orbitPeriod :: OrbitClass t => Getter t (Time Double)
orbitPeriod = to $ do
    sma <- view orbitSemiMajorAxis
    mu <- view orbitMu
    return $ tau * sqrt (sma ^ pos3 / mu)

-- Specific energy

orbitSpecificEnergy :: OrbitClass t => Getter t (SpecificEnergy Double)
orbitSpecificEnergy = to $ do
    mu <- view orbitMu
    sma <- view orbitSemiMajorAxis
    return $ negate $ mu / (_2 * sma)

-- Velocity

orbitVelocity :: OrbitClass t => Getter t OrbitalVelocity
orbitVelocity = to $ do
    h <- view orbitAngularMomentumVector
    mu <- view orbitMu
    e <- view orbitEccentricity
    let v0 = mu / norm h
    return $ OrbitalVelocity (v0 * (_1 - e)) (v0 * (_1 + e))

-- Solar day

bodySolarDay :: Fold Body (Time Double)
bodySolarDay = folding $ do
    orbitM <- views (bodyId . pre bodyOrbit) asKerbolOrbit
    siderealDay <- view bodySiderealRotationPeriod
    return $ do
        orbit <- orbitM
        return $ siderealDay / (_1 - siderealDay / orbit ^. orbitPeriod)
  where
    asKerbolOrbit = mfilter ((== Sun) <$> view orbitBodyId)
