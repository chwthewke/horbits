{-# LANGUAGE TemplateHaskell #-}


module Horbits.Orbit where

import           Control.Lens                         hiding ((*~), _1, _2)
import           Horbits.DimLin
import           Horbits.Quantities
import           Numeric.NumType                      (pos2, pos3)
import           Numeric.Units.Dimensional            (Dimensionless, Length,
                                                       Time, negate, sqrt, tau,
                                                       (*), (*~), (+), (-), (/),
                                                       (^), _1, _2)
import           Numeric.Units.Dimensional.Quantities (GravitationalParameter,
                                                       SpecificEnergy)
import           Numeric.Units.Dimensional.SIUnits
import           Prelude                              hiding (atan2, negate,
                                                       sqrt, (*), (+), (-), (/),
                                                       (^))

data BodyId =
    Kerbol
  | Kerbin
  | Mun
  | Minmus
  | Moho
  | Eve
  | Duna
  | Ike
  | Jool
  | Laythe
  | Vall
  | Bop
  | Tylo
  | Gilly
  | Pol
  | Dres
  | Eeloo
  deriving (Enum, Show, Eq)

getBody :: BodyId -> Body
getBody Kerbin = mkBody 3.5316e12 600000
getBody _ = undefined

mkBody :: Double -> Double -> Body
mkBody mu r = Body (mu *~ (meter ^ pos3 / second ^ pos2)) (r *~ meter)

data Body = Body { _gravitationalParam :: GravitationalParameter Double
                 , _bodyRadius         :: Length Double
                 } deriving (Show, Eq)
makeLenses ''Body

data Orbit = Orbit { _body               :: Body
                   , _angularMomentum    :: SpecificAngularMomentum (V3 Double)
                   , _eccentricityVector :: Dimensionless (V3 Double)
                   , _meanAnomalyAtEpoch :: Dimensionless Double
                   } deriving (Show, Eq)
makeLenses ''Orbit

sma0 :: Orbit -> Length Double
sma0 orbit = quadrance (orbit ^. angularMomentum) / (orbit ^. body . gravitationalParam)

_eccentricity :: Orbit -> Dimensionless Double
_eccentricity orbit = norm $ orbit ^. eccentricityVector

_semiMajorAxis :: Orbit -> Length Double
_semiMajorAxis orbit = sma0 orbit / (_1 - (_eccentricity orbit ^ pos2))

_apoapsis :: Orbit -> Length Double
_apoapsis orbit = sma0 orbit / (_1 - _eccentricity orbit) - orbit ^. body . bodyRadius

_periapsis :: Orbit -> Length Double
_periapsis orbit = sma0 orbit / (_1 + _eccentricity orbit) - orbit ^. body . bodyRadius

_rightAscensionOfAscendingNode  :: Orbit -> Dimensionless Double
_rightAscensionOfAscendingNode orbit = atan2 (orbit ^. angularMomentum . _x) (negate $ orbit ^. angularMomentum . _y)

_inclination :: Orbit -> Dimensionless Double
_inclination orbit = atan2 (norm $ orbit ^. angularMomentum . _xy) (orbit ^. angularMomentum . _z)

_argumentOfPeriapsis :: Orbit -> Dimensionless Double
_argumentOfPeriapsis orbit = atan2 y x
  where y = quadrance (h ^. _xy) * (e ^. _z) - ((h ^. _xy) `dot` (e ^. _xy)) * (h ^. _z)
        x = norm h * (h ^. _x * e ^. _y - h ^. _y * e ^. _x)
        h = orbit ^. angularMomentum
        e = orbit ^. eccentricityVector

_orbitalPeriod :: Orbit -> Time Double
_orbitalPeriod orbit = tau * sqrt (_semiMajorAxis orbit ^ pos3 / orbit ^. body . gravitationalParam)

_specificEnergy :: Orbit -> SpecificEnergy Double
_specificEnergy orbit = negate $ orbit ^. body . gravitationalParam / (_2 * _semiMajorAxis orbit)

