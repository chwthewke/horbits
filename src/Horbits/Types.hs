{-# LANGUAGE TypeFamilies #-}

module Horbits.Types where

import           Control.Lens
import           Numeric.NumType.TF
import           Numeric.Units.Dimensional.TF.Prelude

type DSpecificAngularMomentum = Dim Pos2 Zero Neg1 Zero Zero Zero Zero
type SpecificAngularMomentum = Quantity DSpecificAngularMomentum

newtype AtmosphereHeight = AtmosphereHeight (Length Double) deriving (Show, Eq, Ord)
newtype AtmospherePressure = AtmospherePressure (Pressure Double) deriving (Show, Eq, Ord)
newtype AtmosphereScaleHeight = AtmosphereScaleHeight (Length Double) deriving (Show, Eq, Ord)

newtype BodyGravitationalParam = BodyGravitationalParam (GravitationalParameter Double) deriving (Show, Eq, Ord)
newtype BodyRadius = BodyRadius (Length Double) deriving (Show, Eq, Ord)
newtype BodySoI = BodySoI (Length Double) deriving (Show, Eq, Ord)

newtype OrbitSpecificAngularMomentum t = OrbitSpecificAngularMomentum (SpecificAngularMomentum t) deriving (Show, Eq, Ord)
newtype SemiMajorAxis = SemiMajorAxis (Length Double) deriving (Show, Eq, Ord)
newtype Eccentricity t = Eccentricity (Dimensionless t) deriving (Show, Eq, Ord)
newtype RightAscensionOfAscendingNode = RightAscensionOfAscendingNode (Dimensionless Double) deriving (Show, Eq, Ord)
newtype Inclination = Inclination (Dimensionless Double) deriving (Show, Eq, Ord)
newtype ArgumentOfPeriapsis = ArgumentOfPeriapsis (Dimensionless Double) deriving (Show, Eq, Ord)
newtype MeanAnomalyAtEpoch = MeanAnomalyAtEpoch (Dimensionless Double) deriving (Show, Eq, Ord)
newtype Apoapsis = Apoapsis (Length Double) deriving (Show, Eq, Ord)
newtype Periapsis = Periapsis (Length Double) deriving (Show, Eq, Ord)
newtype OrbitalPeriod = OrbitalPeriod (Time Double) deriving (Show, Eq, Ord)
newtype OrbitSpecificEnergy = OrbitSpecificEnergy (SpecificEnergy Double) deriving (Show, Eq, Ord)

-- TODO Control.Lens.Wrapped

class Measure t where
  type GetValue t :: *
  mkMeasure :: GetValue t -> t
  getValue :: t -> GetValue t
  map :: (GetValue t -> GetValue t) -> t -> t
  map f = mkMeasure . f . getValue

measure :: (Measure t) => Iso' t (GetValue t)
measure = iso getValue mkMeasure

instance Measure (Identity a) where
  type GetValue (Identity a) = a
  mkMeasure = Identity
  getValue = runIdentity

instance Measure AtmosphereHeight where
  type GetValue AtmosphereHeight = Length Double
  mkMeasure = AtmosphereHeight
  getValue (AtmosphereHeight x) = x

instance Measure AtmospherePressure where
  type GetValue AtmospherePressure = Pressure Double
  mkMeasure = AtmospherePressure
  getValue (AtmospherePressure x) = x

instance Measure AtmosphereScaleHeight where
  type GetValue AtmosphereScaleHeight = Length Double
  mkMeasure = AtmosphereScaleHeight
  getValue (AtmosphereScaleHeight x) = x

instance Measure BodyGravitationalParam where
  type GetValue BodyGravitationalParam = GravitationalParameter Double
  mkMeasure = BodyGravitationalParam
  getValue (BodyGravitationalParam x) = x

instance Measure BodyRadius where
  type GetValue BodyRadius = Length Double
  mkMeasure = BodyRadius
  getValue (BodyRadius x) = x

instance Measure BodySoI where
  type GetValue BodySoI = Length Double
  mkMeasure = BodySoI
  getValue (BodySoI x) = x

instance Measure (OrbitSpecificAngularMomentum t) where
  type GetValue (OrbitSpecificAngularMomentum t) = SpecificAngularMomentum t
  mkMeasure = OrbitSpecificAngularMomentum
  getValue (OrbitSpecificAngularMomentum x) = x


instance Measure SemiMajorAxis where
  type GetValue SemiMajorAxis = Length Double
  mkMeasure = SemiMajorAxis
  getValue (SemiMajorAxis x) = x

instance Measure (Eccentricity t) where
  type GetValue (Eccentricity t) = Dimensionless t
  mkMeasure = Eccentricity
  getValue (Eccentricity x) = x

instance Measure RightAscensionOfAscendingNode where
  type GetValue RightAscensionOfAscendingNode = Dimensionless Double
  mkMeasure = RightAscensionOfAscendingNode
  getValue (RightAscensionOfAscendingNode x) = x

instance Measure Inclination where
  type GetValue Inclination = Dimensionless Double
  mkMeasure = Inclination
  getValue (Inclination x) = x

instance Measure ArgumentOfPeriapsis where
  type GetValue ArgumentOfPeriapsis = Dimensionless Double
  mkMeasure = ArgumentOfPeriapsis
  getValue (ArgumentOfPeriapsis x) = x

instance Measure MeanAnomalyAtEpoch where
  type GetValue MeanAnomalyAtEpoch = Dimensionless Double
  mkMeasure = MeanAnomalyAtEpoch
  getValue (MeanAnomalyAtEpoch x) = x

instance Measure Apoapsis where
  type GetValue Apoapsis = Length Double
  mkMeasure = Apoapsis
  getValue (Apoapsis x) = x

instance Measure Periapsis where
  type GetValue Periapsis = Length Double
  mkMeasure = Periapsis
  getValue (Periapsis x) = x

instance Measure OrbitalPeriod where
  type GetValue OrbitalPeriod = Time Double
  mkMeasure = OrbitalPeriod
  getValue (OrbitalPeriod x) = x

instance Measure OrbitSpecificEnergy where
  type GetValue OrbitSpecificEnergy = SpecificEnergy Double
  mkMeasure = OrbitSpecificEnergy
  getValue (OrbitSpecificEnergy x) = x
