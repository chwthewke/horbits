module Horbits.Types where

import           Numeric.NumType.TF
import           Numeric.Units.Dimensional.TF

type DSpecificAngularMomentum = Dim Pos2 Zero Neg1 Zero Zero Zero Zero
type SpecificAngularMomentum = Quantity DSpecificAngularMomentum

newtype SemiMajorAxis = SemiMajorAxis (Length Double)
newtype Eccentricity = Eccentricity (Dimensionless Double)
newtype RightAscensionOfAscendingNode = RightAscensionOfAscendingNode (Dimensionless Double)
newtype Inclination = Inclination (Dimensionless Double)
newtype ArgumentOfPeriapsis = ArgumentOfPeriapsis (Dimensionless Double)
newtype Apoapsis = Apoapsis (Length Double)
newtype Periapsis = Periapsis (Length Double)
