{-# LANGUAGE TypeFamilies #-}

module Horbits.Types where

import           Control.Lens
import           Numeric.NumType.TF
import           Numeric.Units.Dimensional.TF

type DSpecificAngularMomentum = Dim Pos2 Zero Neg1 Zero Zero Zero Zero
type SpecificAngularMomentum = Quantity DSpecificAngularMomentum

newtype SemiMajorAxis = SemiMajorAxis (Length Double) deriving (Show, Eq)
newtype Eccentricity = Eccentricity (Dimensionless Double) deriving (Show, Eq)
newtype RightAscensionOfAscendingNode = RightAscensionOfAscendingNode (Dimensionless Double) deriving (Show, Eq)
newtype Inclination = Inclination (Dimensionless Double) deriving (Show, Eq)
newtype ArgumentOfPeriapsis = ArgumentOfPeriapsis (Dimensionless Double) deriving (Show, Eq)
newtype Apoapsis = Apoapsis (Length Double) deriving (Show, Eq)
newtype Periapsis = Periapsis (Length Double) deriving (Show, Eq)
newtype MeanAnomalyAtEpoch = MeanAnomalyAtEpoch (Dimensionless Double) deriving (Show, Eq)

class Measure t where
  type GetValue t :: *
  mkMeasure :: GetValue t -> t
  getValue :: t -> GetValue t

measure :: (Measure t) => Iso' t (GetValue t)
measure = iso getValue mkMeasure

instance Measure SemiMajorAxis where
  type GetValue SemiMajorAxis = Length Double
  mkMeasure = SemiMajorAxis
  getValue (SemiMajorAxis x) = x

instance Measure Eccentricity where
  type GetValue Eccentricity = Dimensionless Double
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

instance Measure Apoapsis where
  type GetValue Apoapsis = Length Double
  mkMeasure = Apoapsis
  getValue (Apoapsis x) = x

instance Measure Periapsis where
  type GetValue Periapsis = Length Double
  mkMeasure = Periapsis
  getValue (Periapsis x) = x

instance Measure MeanAnomalyAtEpoch where
  type GetValue MeanAnomalyAtEpoch = Dimensionless Double
  mkMeasure = MeanAnomalyAtEpoch
  getValue (MeanAnomalyAtEpoch x) = x

