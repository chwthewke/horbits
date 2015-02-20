{-# LANGUAGE TemplateHaskell #-}


module Horbits.Orbit
    (Orbit(Orbit), orbitBodyId, parentBodyId, angularMomentum, eccentricityVector, meanAnomalyAtEpoch, classical,
    bodyOrbit, orbitBody, orbitMu, eccentricity, semiMajorAxis, apoapsis, periapsis, rightAscensionOfAscendingNode,
    inclination, argumentOfPeriapsis, orbitalPeriod, specificEnergy)
 where

import           Control.Applicative
import           Control.Lens                         hiding ((*~), _1, _2)
import           Horbits.Body
import           Horbits.DimLin
import           Horbits.Types
import           Numeric.Units.Dimensional.TF.Prelude hiding (atan2, map)
import           Prelude                              hiding (atan2, map, negate, pi, sqrt, (*), (+), (-), (/), (^))


data Orbit = Orbit { _orbitBodyId        :: BodyId
                   , _angularMomentum    :: SpecificAngularMomentum (V3 Double)
                   , _eccentricityVector :: Dimensionless (V3 Double)
                   , _meanAnomalyAtEpoch :: Dimensionless Double
                   } deriving (Show, Eq)
makeLenses ''Orbit

classical :: BodyId ->
             Length Double ->
             Dimensionless Double ->
             Dimensionless Double ->
             Dimensionless Double ->
             Dimensionless Double ->
             Dimensionless Double ->
             Orbit
classical b a e lan i p
    = Orbit b (rotate rot $ v3 _0 _0 h) (rotate rot $ v3 e _0 _0)
  where
    h = sqrt $ a * b ^. fromBodyId . bodyGravitationalParam * (_1 - e ^ pos2)
    rot = rotZ lan * rotX i * rotZ p


data ClassicalOrbit = ClassicalOrbit { _cOrbitBodyId                        :: BodyId
                                     , _cOrbitSemiMajorAxis                 :: Length Double
                                     , _cOrbitEccentricity                  :: Dimensionless Double
                                     , _cOrbitRightAscensionOfAscendingNode :: Dimensionless Double
                                     , _cOrbitInclination                   :: Dimensionless Double
                                     , _cOrbitArgumentOfPeriapsis           :: Dimensionless Double
                                     , _cOrbitMeanAnomalyAtEpoch            :: Dimensionless Double
                                     }

makeLensesFor [ ("_cOrbitEccentricity", "cOrbitEccentricity")
              , ("_cOrbitSemiMajorAxis", "cOrbitSemiMajorAxis")
              , ("_cOrbitRightAscensionOfAscendingNode", "cOrbitRightAscensionOfAscendingNode")
              , ("_cOrbitInclination", "cOrbitInclination")
              , ("_cOrbitArgumentOfPeriapsis", "cOrbitArgumentOfPeriapsis")
              ] ''ClassicalOrbit

orbitToClassical :: Orbit -> ClassicalOrbit
orbitToClassical = ClassicalOrbit <$> _orbitBodyId
                                  <*> _semiMajorAxis
                                  <*> _eccentricity
                                  <*> _rightAscensionOfAscendingNode
                                  <*> _inclination
                                  <*> _argumentOfPeriapsis
                                  <*> _meanAnomalyAtEpoch

classicalToOrbit :: ClassicalOrbit -> Orbit
classicalToOrbit = classical <$> _cOrbitBodyId
                             <*> _cOrbitSemiMajorAxis
                             <*> _cOrbitEccentricity
                             <*> _cOrbitRightAscensionOfAscendingNode
                             <*> _cOrbitInclination
                             <*> _cOrbitArgumentOfPeriapsis
                             <*> _cOrbitMeanAnomalyAtEpoch

classicalIso :: Iso' Orbit ClassicalOrbit
classicalIso = iso orbitToClassical classicalToOrbit

_bodyOrbit :: BodyId -> Maybe Orbit
_bodyOrbit Kerbol = Nothing
_bodyOrbit Moho   = Just $ classical Kerbol (5263138304 *~ meter)
                                            (0.2 *~ one)
                                            (70 *~ degree)
                                            (7 *~ degree)
                                            (15 *~ degree)
                                            (3.14 *~ radian)
_bodyOrbit Eve    = Just $ classical Kerbol (9832684544 *~ meter)
                                            (0.01 *~ one)
                                            (15 *~ degree)
                                            (2.1 *~ degree)
                                            _0
                                            (3.14 *~ radian)
_bodyOrbit Gilly  = Just $ classical Eve    (31500000 *~ meter)
                                            (0.55 *~ one)
                                            (80 *~ degree)
                                            (12 *~ degree)
                                            (10 *~ degree)
                                            (0.9 *~ radian)
_bodyOrbit Kerbin = Just $ classical Kerbol (13599840256 *~ meter)
                                            _0
                                            _0
                                            _0
                                            _0
                                            (3.14 *~ radian)
_bodyOrbit Mun    = Just $ classical Kerbin (12000000 *~ meter)
                                            _0
                                            _0
                                            _0
                                            _0
                                            (1.7 *~ radian)
_bodyOrbit Minmus = Just $ classical Kerbin (47000000 *~ meter)
                                            _0
                                            (78 *~ degree)
                                            (6 *~ degree)
                                            (38 *~ degree)
                                            (0.9 *~ radian)
_bodyOrbit Duna   = Just $ classical Kerbol (20726155264 *~ meter)
                                            (0.05 *~ one)
                                            (135.5 *~ degree)
                                            (0.06 *~ degree)
                                            _0
                                            (3.14 *~ radian)
_bodyOrbit Ike    = Just $ classical Duna   (3200000 *~ meter)
                                            (0.03 *~ one)
                                            _0
                                            (0.2 *~ degree)
                                            _0
                                            (1.7 *~ radian)
_bodyOrbit Dres   = Just $ classical Kerbol (40839348203 *~ meter)
                                            (0.14 *~ one)
                                            (280 *~ degree)
                                            (5 *~ degree)
                                            (90 *~ degree)
                                            (3.14 *~ radian)
_bodyOrbit Jool   = Just $ classical Kerbol (68773560320 *~ meter)
                                            (0.05 *~ one)
                                            (52 *~ degree)
                                            (1.304 *~ degree)
                                            _0
                                            (0.1 *~ radian)
_bodyOrbit Laythe = Just $ classical Jool   (27184000 *~ meter)
                                            _0
                                            _0
                                            _0
                                            _0
                                            (3.14 *~ radian)
_bodyOrbit Vall   = Just $ classical Jool   (43152000 *~ meter)
                                            _0
                                            _0
                                            _0
                                            _0
                                            (0.9 *~ radian)
_bodyOrbit Tylo   = Just $ classical Jool   (68500000 *~ meter)
                                            _0
                                            _0
                                            _0
                                            _0
                                            (3.14 *~ radian)
_bodyOrbit Bop    = Just $ classical Jool   (128500000 *~ meter)
                                            (0.24 *~ one)
                                            (10 *~ degree)
                                            (15 *~ degree)
                                            (25 *~ degree)
                                            (0.9 *~ radian)
_bodyOrbit Pol    = Just $ classical Jool   (179890000 *~ meter)
                                            (0.17 *~ one)
                                            (2 *~ degree)
                                            (4.25 *~ degree)
                                            (15 *~ degree)
                                            (0.9 *~ radian)
_bodyOrbit Eeloo  = Just $ classical Kerbol (90118820000 *~ meter)
                                            (0.26 *~ one)
                                            (50 *~ degree)
                                            (6.15 *~ degree)
                                            (260 *~ degree)
                                            (3.14 *~ radian)

bodyOrbit :: Fold BodyId Orbit
bodyOrbit = folding _bodyOrbit

parentBodyId :: Fold Body BodyId
parentBodyId = bodyId . bodyOrbit . orbitBodyId

orbitBody :: Getter Orbit Body
orbitBody = orbitBodyId . fromBodyId

orbitMu :: Getter Orbit (GravitationalParameter Double)
orbitMu = orbitBody . bodyGravitationalParam

sma0 :: Orbit -> Length Double
sma0 orbit = quadrance (orbit ^. angularMomentum) / (orbit ^. orbitMu)

eccentricityM :: Orbit -> Dimensionless Double
eccentricityM orbit =  norm $ orbit ^. eccentricityVector

_eccentricity :: Orbit -> Dimensionless Double
_eccentricity orbit = eccentricityM orbit

-- TODO keep sma, except if sign (e - 1) changes, then what?
_setEccentricity :: Orbit -> Dimensionless Double -> Orbit
_setEccentricity orbit e = orbit & classicalIso . cOrbitEccentricity .~ e

eccentricity :: Lens' Orbit (Dimensionless Double)
eccentricity = lens _eccentricity _setEccentricity

_semiMajorAxis :: Orbit -> Length Double
_semiMajorAxis orbit = sma0 orbit / (_1 - (eccentricityM orbit ^ pos2))

_setSemiMajorAxis :: Orbit -> Length Double -> Orbit
_setSemiMajorAxis orbit sma = orbit & classicalIso . cOrbitSemiMajorAxis .~ sma

semiMajorAxis :: Lens' Orbit (Length Double)
semiMajorAxis = lens _semiMajorAxis _setSemiMajorAxis

_apoapsis :: Orbit -> Length Double
_apoapsis orbit = sma0 orbit / (_1 - eccentricityM orbit) - orbit ^. orbitBody . bodyRadius

_setApoapsis :: Orbit -> Length Double -> Orbit
_setApoapsis orbit ap = orbit & semiMajorAxis .~ sma
                              & eccentricity .~ e
  where
    sma = orbit ^. semiMajorAxis + (ap - _apoapsis orbit)
    e = (ap - _periapsis orbit) / (_2 * sma)

apoapsis :: Lens' Orbit (Length Double)
apoapsis = lens _apoapsis _setApoapsis

_periapsis :: Orbit -> Length Double
_periapsis orbit = sma0 orbit / (_1 + eccentricityM orbit) - orbit ^. orbitBody . bodyRadius

_setPeriapsis :: Orbit -> Length Double -> Orbit
_setPeriapsis orbit pe = orbit & semiMajorAxis .~ sma
                               & eccentricity .~ e
  where
    sma = orbit ^. semiMajorAxis + (pe - _periapsis orbit)
    e = (_apoapsis orbit - pe) / (_2 * sma)

periapsis :: Lens' Orbit (Length Double)
periapsis = lens _periapsis _setPeriapsis


_rightAscensionOfAscendingNode :: Orbit -> Dimensionless Double
_rightAscensionOfAscendingNode orbit =
    atan2 (orbit ^. angularMomentum . _x) (negate $ orbit ^. angularMomentum . _y)

_setRightAscensionOfAscendingNode :: Orbit -> Dimensionless Double -> Orbit
_setRightAscensionOfAscendingNode orbit raan = orbit & classicalIso . cOrbitRightAscensionOfAscendingNode .~ raan

rightAscensionOfAscendingNode :: Lens' Orbit (Dimensionless Double)
rightAscensionOfAscendingNode = lens _rightAscensionOfAscendingNode _setRightAscensionOfAscendingNode

_inclination :: Orbit -> Dimensionless Double
_inclination orbit =
    atan2 (norm $ orbit ^. angularMomentum . _xy) (orbit ^. angularMomentum . _z)

_setInclination :: Orbit -> Dimensionless Double -> Orbit
_setInclination orbit incl = orbit & classicalIso . cOrbitInclination .~ incl

inclination :: Lens' Orbit (Dimensionless Double)
inclination = lens _inclination _setInclination

_argumentOfPeriapsis :: Orbit -> Dimensionless Double
_argumentOfPeriapsis orbit = atan2 y x
  where
    y = quadrance (h ^. _xy) * (e ^. _z) - ((h ^. _xy) `dot` (e ^. _xy)) * (h ^. _z)
    x = norm h * (h ^. _x * e ^. _y - h ^. _y * e ^. _x)
    h = orbit ^. angularMomentum
    e = orbit ^. eccentricityVector

_setArgumentOfPeriapsis :: Orbit -> Dimensionless Double -> Orbit
_setArgumentOfPeriapsis orbit argpe = orbit & classicalIso . cOrbitArgumentOfPeriapsis .~ argpe

argumentOfPeriapsis :: Lens' Orbit (Dimensionless Double)
argumentOfPeriapsis = lens _argumentOfPeriapsis _setArgumentOfPeriapsis

_orbitalPeriod :: Orbit -> Time Double
_orbitalPeriod orbit = tau * sqrt (sma ^ pos3 / orbit ^. orbitMu)
  where
    sma = orbit ^. semiMajorAxis

orbitalPeriod :: Getter Orbit (Time Double)
orbitalPeriod = to _orbitalPeriod

_specificEnergy :: Orbit -> SpecificEnergy Double
_specificEnergy orbit = negate $ orbit ^. orbitMu / (_2 * sma)
  where
    sma = orbit ^. semiMajorAxis

specificEnergy :: Getter Orbit (SpecificEnergy Double)
specificEnergy = to _specificEnergy
