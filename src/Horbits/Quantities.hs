module Horbits.Quantities where

import           Numeric.NumType
import           Numeric.Units.Dimensional

type DReducedAngularMomentum = Dim Pos2 Zero Neg1 Zero Zero Zero Zero
type ReducedAngularMomentum = Quantity DReducedAngularMomentum
