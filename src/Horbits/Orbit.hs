{-# LANGUAGE TemplateHaskell #-}


module Horbits.Orbit
                     (Orbit(Orbit), orbitBodyId, angularMomentum, eccentricityVector, meanAnomalyAtEpoch,
                      classical, bodyOrbit, orbitBody, orbitMu, _eccentricity, _semiMajorAxis, _apoapsis, _periapsis,
                      _rightAscensionOfAscendingNode, _inclination, _argumentOfPeriapsis, _orbitalPeriod, _specificEnergy)
 where

import           Control.Lens                         hiding ((*~), _1, _2)
import           Horbits.Body
import           Horbits.DimLin
import           Horbits.Types
import           Numeric.Units.Dimensional.TF.Prelude hiding (atan2)
import           Prelude                              hiding (atan2, negate, pi,
                                                       sqrt, (*), (+), (-), (/),
                                                       (^))


data Orbit = Orbit { _orbitBodyId        :: BodyId
                   , _angularMomentum    :: SpecificAngularMomentum (V3 Double)
                   , _eccentricityVector :: Dimensionless (V3 Double)
                   , _meanAnomalyAtEpoch :: MeanAnomalyAtEpoch
                   } deriving (Show, Eq)
makeLenses ''Orbit

classical :: BodyId ->
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
  where h = sqrt $ a * b ^. fromBodyId . bodyGravitationalParam * (_1 - e ^ pos2)
        rot = rotZ lan * rotX i * rotZ p


bodyOrbit :: BodyId -> Maybe Orbit
bodyOrbit Kerbol = Nothing
bodyOrbit Moho   = Just $ classical Kerbol (SemiMajorAxis $ 5263138304 *~ meter)
                                           (Eccentricity $ 0.2 *~ one)
                                           (RightAscensionOfAscendingNode $ 70 *~ degree)
                                           (Inclination $ 7 *~ degree)
                                           (ArgumentOfPeriapsis $ 15 *~ degree)
                                           (MeanAnomalyAtEpoch $ 3.14 *~ radian)
bodyOrbit Eve    = Just $ classical Kerbol (SemiMajorAxis $ 9832684544 *~ meter)
                                           (Eccentricity $ 0.01 *~ one)
                                           (RightAscensionOfAscendingNode $ 15 *~ degree)
                                           (Inclination $ 2.1 *~ degree)
                                           (ArgumentOfPeriapsis _0)
                                           (MeanAnomalyAtEpoch $ 3.14 *~ radian)
bodyOrbit Gilly  = Just $ classical Eve    (SemiMajorAxis $ 31500000 *~ meter)
                                           (Eccentricity $ 0.55 *~ one)
                                           (RightAscensionOfAscendingNode $ 80 *~ degree)
                                           (Inclination $ 12 *~ degree)
                                           (ArgumentOfPeriapsis $ 10 *~ degree)
                                           (MeanAnomalyAtEpoch $ 0.9 *~ radian)
bodyOrbit Kerbin = Just $ classical Kerbol (SemiMajorAxis $ 13599840256 *~ meter)
                                           (Eccentricity _0)
                                           (RightAscensionOfAscendingNode _0)
                                           (Inclination _0)
                                           (ArgumentOfPeriapsis _0)
                                           (MeanAnomalyAtEpoch $ 3.14 *~ radian)
bodyOrbit Mun    = Just $ classical Kerbin (SemiMajorAxis $ 12000000 *~ meter)
                                           (Eccentricity _0)
                                           (RightAscensionOfAscendingNode _0)
                                           (Inclination _0)
                                           (ArgumentOfPeriapsis _0)
                                           (MeanAnomalyAtEpoch $ 1.7 *~ radian)
bodyOrbit Minmus = Just $ classical Kerbin (SemiMajorAxis $ 47000000 *~ meter)
                                           (Eccentricity _0)
                                           (RightAscensionOfAscendingNode $ 78 *~ degree)
                                           (Inclination $ 6 *~ degree)
                                           (ArgumentOfPeriapsis $ 38 *~ degree)
                                           (MeanAnomalyAtEpoch $ 0.9 *~ radian)
bodyOrbit Duna   = Just $ classical Kerbol (SemiMajorAxis $ 20726155264 *~ meter)
                                           (Eccentricity $ 0.05 *~ one)
                                           (RightAscensionOfAscendingNode $ 135.5 *~ degree)
                                           (Inclination $ 0.06 *~ degree)
                                           (ArgumentOfPeriapsis _0)
                                           (MeanAnomalyAtEpoch $ 3.14 *~ radian)
bodyOrbit Ike    = Just $ classical Duna   (SemiMajorAxis $ 3200000 *~ meter)
                                           (Eccentricity $ 0.03 *~ one)
                                           (RightAscensionOfAscendingNode _0)
                                           (Inclination $ 0.2 *~ degree)
                                           (ArgumentOfPeriapsis _0)
                                           (MeanAnomalyAtEpoch $ 1.7 *~ radian)
bodyOrbit Dres   = Just $ classical Kerbol (SemiMajorAxis $ 40839348203 *~ meter)
                                           (Eccentricity $ 0.14 *~ one)
                                           (RightAscensionOfAscendingNode $ 280 *~ degree)
                                           (Inclination $ 5 *~ degree)
                                           (ArgumentOfPeriapsis $ 90 *~ degree)
                                           (MeanAnomalyAtEpoch $ 3.14 *~ radian)
bodyOrbit Jool   = Just $ classical Kerbol (SemiMajorAxis $ 68773560320 *~ meter)
                                           (Eccentricity $ 0.05 *~ one)
                                           (RightAscensionOfAscendingNode $ 52 *~ degree)
                                           (Inclination $ 1.304 *~ degree)
                                           (ArgumentOfPeriapsis _0)
                                           (MeanAnomalyAtEpoch $ 0.1 *~ radian)
bodyOrbit Laythe = Just $ classical Jool   (SemiMajorAxis $ 27184000 *~ meter)
                                           (Eccentricity _0)
                                           (RightAscensionOfAscendingNode _0)
                                           (Inclination _0)
                                           (ArgumentOfPeriapsis _0)
                                           (MeanAnomalyAtEpoch $ 3.14 *~ radian)
bodyOrbit Vall   = Just $ classical Jool   (SemiMajorAxis $ 43152000 *~ meter)
                                           (Eccentricity _0)
                                           (RightAscensionOfAscendingNode _0)
                                           (Inclination _0)
                                           (ArgumentOfPeriapsis _0)
                                           (MeanAnomalyAtEpoch $ 0.9 *~ radian)
bodyOrbit Tylo   = Just $ classical Jool   (SemiMajorAxis $ 68500000 *~ meter)
                                           (Eccentricity _0)
                                           (RightAscensionOfAscendingNode _0)
                                           (Inclination _0)
                                           (ArgumentOfPeriapsis _0)
                                           (MeanAnomalyAtEpoch $ 3.14 *~ radian)
bodyOrbit Bop    = Just $ classical Jool   (SemiMajorAxis $ 128500000 *~ meter)
                                           (Eccentricity $ 0.24 *~ one)
                                           (RightAscensionOfAscendingNode $ 10 *~ degree)
                                           (Inclination $ 15 *~ degree)
                                           (ArgumentOfPeriapsis $ 25 *~ degree)
                                           (MeanAnomalyAtEpoch $ 0.9 *~ radian)
bodyOrbit Pol    = Just $ classical Jool   (SemiMajorAxis $ 179890000 *~ meter)
                                           (Eccentricity $ 0.17 *~ one)
                                           (RightAscensionOfAscendingNode $ 2 *~ degree)
                                           (Inclination $ 4.25 *~ degree)
                                           (ArgumentOfPeriapsis $ 15 *~ degree)
                                           (MeanAnomalyAtEpoch $ 0.9 *~ radian)
bodyOrbit Eeloo  = Just $ classical Kerbol (SemiMajorAxis $ 90118820000 *~ meter)
                                           (Eccentricity $ 0.26 *~ one)
                                           (RightAscensionOfAscendingNode $ 50 *~ degree)
                                           (Inclination $ 6.15 *~ degree)
                                           (ArgumentOfPeriapsis $ 260 *~ degree)
                                           (MeanAnomalyAtEpoch $ 3.14 *~ radian)



orbitBody :: Getter Orbit Body
orbitBody = orbitBodyId . fromBodyId

orbitMu :: Getter Orbit (GravitationalParameter Double)
orbitMu = orbitBody . bodyGravitationalParam

sma0 :: Orbit -> Length Double
sma0 orbit = quadrance (orbit ^. angularMomentum) / (orbit ^. orbitMu)

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
_orbitalPeriod orbit = tau * sqrt (sma ^ pos3 / orbit ^. orbitMu)
  where SemiMajorAxis sma = _semiMajorAxis orbit

_specificEnergy :: Orbit -> SpecificEnergy Double
_specificEnergy orbit = negate $ orbit ^. orbitMu / (_2 * sma)
  where SemiMajorAxis sma = _semiMajorAxis orbit

