module Horbits.Quantities where

import           Numeric.NumType
import           Numeric.Units.Dimensional

type DSpecificAngularMomentum = Dim Pos2 Zero Neg1 Zero Zero Zero Zero
type SpecificAngularMomentum = Quantity DSpecificAngularMomentum
