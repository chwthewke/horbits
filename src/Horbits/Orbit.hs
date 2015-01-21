{-# LANGUAGE TemplateHaskell #-}


module Horbits.Orbit where

import           Control.Lens                            hiding ((*~), _1, _2)
import           Horbits.Body
import           Horbits.DimLin
import           Horbits.Types
import           Numeric.NumType.TF                      (pos2, pos3)
import           Numeric.Units.Dimensional.TF            (Dimensionless, Length,
                                                          Time, negate, sqrt,
                                                          tau, (*), (+), (-),
                                                          (/), (^), _0, _1, _2)
import           Numeric.Units.Dimensional.TF.Quantities (SpecificEnergy)
import           Prelude                                 hiding (atan2, negate,
                                                          sqrt, (*), (+), (-),
                                                          (/), (^))


data Orbit = Orbit { _orbitBody          :: Body
                   , _angularMomentum    :: SpecificAngularMomentum (V3 Double)
                   , _eccentricityVector :: Dimensionless (V3 Double)
                   , _meanAnomalyAtEpoch :: MeanAnomalyAtEpoch
                   } deriving (Show, Eq)
makeLenses ''Orbit

sma0 :: Orbit -> Length Double
sma0 orbit = quadrance (orbit ^. angularMomentum) / (orbit ^. orbitBody . gravitationalParam)

eccentricityM :: Orbit -> Dimensionless Double
eccentricityM orbit =  norm $ orbit ^. eccentricityVector

_eccentricity :: Orbit -> Eccentricity
_eccentricity orbit = mkMeasure $ eccentricityM orbit

_semiMajorAxis :: Orbit -> SemiMajorAxis
_semiMajorAxis orbit = mkMeasure $ sma0 orbit / (_1 - (eccentricityM orbit ^ pos2))

_apoapsis :: Orbit -> Apoapsis
_apoapsis orbit = mkMeasure $ sma0 orbit / (_1 - eccentricityM orbit) - orbit ^. orbitBody . bodyRadius

_periapsis :: Orbit -> Periapsis
_periapsis orbit = mkMeasure $ sma0 orbit / (_1 + eccentricityM orbit) - orbit ^. orbitBody . bodyRadius

_rightAscensionOfAscendingNode  :: Orbit -> RightAscensionOfAscendingNode
_rightAscensionOfAscendingNode orbit =
  mkMeasure $
    atan2 (orbit ^. angularMomentum . _x) (negate $ orbit ^. angularMomentum . _y)

_inclination :: Orbit -> Inclination
_inclination orbit = mkMeasure $ atan2 (norm $ orbit ^. angularMomentum . _xy) (orbit ^. angularMomentum . _z)

_argumentOfPeriapsis :: Orbit -> ArgumentOfPeriapsis
_argumentOfPeriapsis orbit = mkMeasure $ atan2 y x
  where y = quadrance (h ^. _xy) * (e ^. _z) - ((h ^. _xy) `dot` (e ^. _xy)) * (h ^. _z)
        x = norm h * (h ^. _x * e ^. _y - h ^. _y * e ^. _x)
        h = orbit ^. angularMomentum
        e = orbit ^. eccentricityVector

_orbitalPeriod :: Orbit -> Time Double
_orbitalPeriod orbit = tau * sqrt (sma ^ pos3 / orbit ^. orbitBody . gravitationalParam)
  where SemiMajorAxis sma = _semiMajorAxis orbit

_specificEnergy :: Orbit -> SpecificEnergy Double
_specificEnergy orbit = negate $ orbit ^. orbitBody . gravitationalParam / (_2 * sma)
  where SemiMajorAxis sma = _semiMajorAxis orbit

classical :: Body ->
              SemiMajorAxis ->
              Eccentricity ->
              RightAscensionOfAscendingNode ->
              Inclination ->
              ArgumentOfPeriapsis ->
              MeanAnomalyAtEpoch ->
                Orbit
classical b
  (SemiMajorAxis a)
  (Eccentricity e)
  (RightAscensionOfAscendingNode lan)
  (Inclination i)
  (ArgumentOfPeriapsis p)
    = Orbit b (rotate rot $ v3 _0 _0 h) (rotate rot $ v3 e _0 _0)
  where h = sqrt $ a * b ^. gravitationalParam * (_1 - e ^ pos2)
        rot = rotZ lan * rotX i * rotZ p
