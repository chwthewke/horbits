module Horbits.Types where

import           Numeric.Units.Dimensional.TF

newtype SemiMajorAxis = SemiMajorAxis (Length Double)
newtype Eccentricity = Eccentricity (Dimensionless Double)
newtype RightAscensionOfAscendingNode = RightAscensionOfAscendingNode (Dimensionless Double)
newtype Inclination = Inclination (Dimensionless Double)
newtype ArgumentOfPeriapsis = ArgumentOfPeriapsis (Dimensionless Double)
newtype Apoapsis = Apoapsis (Length Double)
newtype Periapsis = Periapsis (Length Double)
