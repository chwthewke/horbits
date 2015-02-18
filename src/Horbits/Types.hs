{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Horbits.Types where

import           Control.Lens
import           Numeric.NumType.TF
import           Numeric.Units.Dimensional.TF.Prelude

type DSpecificAngularMomentum = Dim Pos2 Zero Neg1 Zero Zero Zero Zero
type SpecificAngularMomentum = Quantity DSpecificAngularMomentum

newtype AtmosphereHeight = AtmosphereHeight (Length Double) deriving (Show, Eq, Ord)
makeWrapped ''AtmosphereHeight

newtype AtmospherePressure = AtmospherePressure (Pressure Double) deriving (Show, Eq, Ord)
makeWrapped ''AtmospherePressure

newtype AtmosphereScaleHeight = AtmosphereScaleHeight (Length Double) deriving (Show, Eq, Ord)
makeWrapped ''AtmosphereScaleHeight


newtype BodyGravitationalParam = BodyGravitationalParam (GravitationalParameter Double) deriving (Show, Eq, Ord)
makeWrapped ''BodyGravitationalParam

newtype BodyRadius = BodyRadius (Length Double) deriving (Show, Eq, Ord)
makeWrapped ''BodyRadius

newtype BodySiderealRotationPeriod = BodySiderealRotationPeriod (Time Double) deriving (Show, Eq, Ord)
makeWrapped ''BodySiderealRotationPeriod

newtype BodySoI = BodySoI (Length Double) deriving (Show, Eq, Ord)
makeWrapped ''BodySoI

newtype BodySurfaceArea = BodySurfaceArea (Area Double) deriving (Show, Eq, Ord)
makeWrapped ''BodySurfaceArea

newtype BodyMass = BodyMass (Mass Double) deriving (Show, Eq, Ord)
makeWrapped ''BodyMass

newtype BodyDensity = BodyDensity (Density Double) deriving (Show, Eq, Ord)
makeWrapped ''BodyDensity

newtype BodySurfaceGravity = BodySurfaceGravity (Acceleration Double) deriving (Show, Eq, Ord)
makeWrapped ''BodySurfaceGravity

newtype BodyEscapeVelocity = BodyEscapeVelocity (Velocity Double) deriving (Show, Eq, Ord)
makeWrapped ''BodyEscapeVelocity

newtype BodySynchronousOrbitAltitude = BodySynchronousOrbitAltitude (Length Double) deriving (Show, Eq, Ord)
makeWrapped ''BodySynchronousOrbitAltitude

newtype OrbitSpecificAngularMomentum t = OrbitSpecificAngularMomentum (SpecificAngularMomentum t)
    deriving (Show, Eq, Ord)
makeWrapped ''OrbitSpecificAngularMomentum

newtype SemiMajorAxis = SemiMajorAxis (Length Double) deriving (Show, Eq, Ord)
makeWrapped ''SemiMajorAxis

newtype Eccentricity t = Eccentricity (Dimensionless t) deriving (Show, Eq, Ord)
makeWrapped ''Eccentricity

newtype RightAscensionOfAscendingNode = RightAscensionOfAscendingNode (Dimensionless Double) deriving (Show, Eq, Ord)
makeWrapped ''RightAscensionOfAscendingNode

newtype Inclination = Inclination (Dimensionless Double) deriving (Show, Eq, Ord)
makeWrapped ''Inclination

newtype ArgumentOfPeriapsis = ArgumentOfPeriapsis (Dimensionless Double) deriving (Show, Eq, Ord)
makeWrapped ''ArgumentOfPeriapsis

newtype MeanAnomalyAtEpoch = MeanAnomalyAtEpoch (Dimensionless Double) deriving (Show, Eq, Ord)
makeWrapped ''MeanAnomalyAtEpoch

newtype Apoapsis = Apoapsis (Length Double) deriving (Show, Eq, Ord)
makeWrapped ''Apoapsis

newtype Periapsis = Periapsis (Length Double) deriving (Show, Eq, Ord)
makeWrapped ''Periapsis

newtype OrbitalPeriod = OrbitalPeriod (Time Double) deriving (Show, Eq, Ord)
makeWrapped ''OrbitalPeriod

newtype OrbitSpecificEnergy = OrbitSpecificEnergy (SpecificEnergy Double) deriving (Show, Eq, Ord)
makeWrapped ''OrbitSpecificEnergy


