module Horbits.Quantities where

import           Numeric.NumType.TF
import           Numeric.Units.Dimensional.TF

type DSpecificAngularMomentum = Dim Pos2 Zero Neg1 Zero Zero Zero Zero
type SpecificAngularMomentum = Quantity DSpecificAngularMomentum
