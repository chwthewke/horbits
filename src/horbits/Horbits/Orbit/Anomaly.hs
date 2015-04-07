module Horbits.Orbit.Anomaly where

import           Control.Applicative
import           Control.Monad
import           Data.List.NonEmpty           as NE
import           Linear.Epsilon
import           Numeric.Units.Dimensional.TF (Dimensional (..), Dimensionless)

class Anomaly a where
    eccentricity :: a -> Dimensionless Double

    meanAnomaly :: a -> Dimensionless Double
    meanAnomaly a = Dimensional $ ea - e * sin ea
      where
        Dimensional e = eccentricity a
        Dimensional ea = eccentricAnomaly a

    eccentricAnomaly :: a -> Dimensionless Double
    eccentricAnomaly a = Dimensional . NE.last $ NE.unfold next e0
      where
        Dimensional e = eccentricity a
        Dimensional m = meanAnomaly a
        dnext en = (en - e * sin en - m) / (1 - e * cos en)
        next en = (en, (en -) <$> mfilter (not . nearZero) (Just $ dnext en))
        e0 = if e > 0.8 then pi else m

    trueAnomaly :: a -> Dimensionless Double
    trueAnomaly a = Dimensional $ 2 * atan (sqrt ((1 + e) / (1 - e)) * tan (ea / 2))
      where
        Dimensional e = eccentricity a
        Dimensional ea = eccentricAnomaly a

data MeanAnomaly = MeanAnomaly (Dimensionless Double) (Dimensionless Double)

instance Anomaly MeanAnomaly where
    eccentricity (MeanAnomaly e _) = e
    meanAnomaly (MeanAnomaly _ m) = m

data EccentricAnomaly = EccentricAnomaly (Dimensionless Double) (Dimensionless Double)

instance Anomaly EccentricAnomaly where
    eccentricity (EccentricAnomaly e _) = e
    eccentricAnomaly (EccentricAnomaly _ ea) = ea


