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
                   , _angularMomentum    :: OrbitSpecificAngularMomentum (V3 Double)
                   , _eccentricityVector :: Eccentricity (V3 Double)
                   , _meanAnomalyAtEpoch :: MeanAnomalyAtEpoch
                   } deriving (Show, Eq)
makeLenses ''Orbit

classical :: BodyId ->
             SemiMajorAxis ->
             Eccentricity Double ->
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
    = Orbit b (v3' _0 _0 h & _Wrapped' %~ rotate rot) (v3' e _0 _0 & _Wrapped' %~ rotate rot)
  where
    h = sqrt $ a * b ^. fromBodyId . bodyGravitationalParam . _Wrapped' * (_1 - e ^ pos2)
    rot = rotZ lan * rotX i * rotZ p


data ClassicalOrbit = ClassicalOrbit { _cOrbitBodyId                        :: BodyId
                                     , _cOrbitSemiMajorAxis                 :: SemiMajorAxis
                                     , _cOrbitEccentricity                  :: Eccentricity Double
                                     , _cOrbitRightAscensionOfAscendingNode :: RightAscensionOfAscendingNode
                                     , _cOrbitInclination                   :: Inclination
                                     , _cOrbitArgumentOfPeriapsis           :: ArgumentOfPeriapsis
                                     , _cOrbitMeanAnomalyAtEpoch            :: MeanAnomalyAtEpoch
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
_bodyOrbit Moho   = Just $ classical Kerbol (SemiMajorAxis $ 5263138304 *~ meter)
                                            (Eccentricity $ 0.2 *~ one)
                                            (RightAscensionOfAscendingNode $ 70 *~ degree)
                                            (Inclination $ 7 *~ degree)
                                            (ArgumentOfPeriapsis $ 15 *~ degree)
                                            (MeanAnomalyAtEpoch $ 3.14 *~ radian)
_bodyOrbit Eve    = Just $ classical Kerbol (SemiMajorAxis $ 9832684544 *~ meter)
                                            (Eccentricity $ 0.01 *~ one)
                                            (RightAscensionOfAscendingNode $ 15 *~ degree)
                                            (Inclination $ 2.1 *~ degree)
                                            (ArgumentOfPeriapsis _0)
                                            (MeanAnomalyAtEpoch $ 3.14 *~ radian)
_bodyOrbit Gilly  = Just $ classical Eve    (SemiMajorAxis $ 31500000 *~ meter)
                                            (Eccentricity $ 0.55 *~ one)
                                            (RightAscensionOfAscendingNode $ 80 *~ degree)
                                            (Inclination $ 12 *~ degree)
                                            (ArgumentOfPeriapsis $ 10 *~ degree)
                                            (MeanAnomalyAtEpoch $ 0.9 *~ radian)
_bodyOrbit Kerbin = Just $ classical Kerbol (SemiMajorAxis $ 13599840256 *~ meter)
                                            (Eccentricity _0)
                                            (RightAscensionOfAscendingNode _0)
                                            (Inclination _0)
                                            (ArgumentOfPeriapsis _0)
                                            (MeanAnomalyAtEpoch $ 3.14 *~ radian)
_bodyOrbit Mun    = Just $ classical Kerbin (SemiMajorAxis $ 12000000 *~ meter)
                                            (Eccentricity _0)
                                            (RightAscensionOfAscendingNode _0)
                                            (Inclination _0)
                                            (ArgumentOfPeriapsis _0)
                                            (MeanAnomalyAtEpoch $ 1.7 *~ radian)
_bodyOrbit Minmus = Just $ classical Kerbin (SemiMajorAxis $ 47000000 *~ meter)
                                            (Eccentricity _0)
                                            (RightAscensionOfAscendingNode $ 78 *~ degree)
                                            (Inclination $ 6 *~ degree)
                                            (ArgumentOfPeriapsis $ 38 *~ degree)
                                            (MeanAnomalyAtEpoch $ 0.9 *~ radian)
_bodyOrbit Duna   = Just $ classical Kerbol (SemiMajorAxis $ 20726155264 *~ meter)
                                            (Eccentricity $ 0.05 *~ one)
                                            (RightAscensionOfAscendingNode $ 135.5 *~ degree)
                                            (Inclination $ 0.06 *~ degree)
                                            (ArgumentOfPeriapsis _0)
                                            (MeanAnomalyAtEpoch $ 3.14 *~ radian)
_bodyOrbit Ike    = Just $ classical Duna   (SemiMajorAxis $ 3200000 *~ meter)
                                            (Eccentricity $ 0.03 *~ one)
                                            (RightAscensionOfAscendingNode _0)
                                            (Inclination $ 0.2 *~ degree)
                                            (ArgumentOfPeriapsis _0)
                                            (MeanAnomalyAtEpoch $ 1.7 *~ radian)
_bodyOrbit Dres   = Just $ classical Kerbol (SemiMajorAxis $ 40839348203 *~ meter)
                                            (Eccentricity $ 0.14 *~ one)
                                            (RightAscensionOfAscendingNode $ 280 *~ degree)
                                            (Inclination $ 5 *~ degree)
                                            (ArgumentOfPeriapsis $ 90 *~ degree)
                                            (MeanAnomalyAtEpoch $ 3.14 *~ radian)
_bodyOrbit Jool   = Just $ classical Kerbol (SemiMajorAxis $ 68773560320 *~ meter)
                                            (Eccentricity $ 0.05 *~ one)
                                            (RightAscensionOfAscendingNode $ 52 *~ degree)
                                            (Inclination $ 1.304 *~ degree)
                                            (ArgumentOfPeriapsis _0)
                                            (MeanAnomalyAtEpoch $ 0.1 *~ radian)
_bodyOrbit Laythe = Just $ classical Jool   (SemiMajorAxis $ 27184000 *~ meter)
                                            (Eccentricity _0)
                                            (RightAscensionOfAscendingNode _0)
                                            (Inclination _0)
                                            (ArgumentOfPeriapsis _0)
                                            (MeanAnomalyAtEpoch $ 3.14 *~ radian)
_bodyOrbit Vall   = Just $ classical Jool   (SemiMajorAxis $ 43152000 *~ meter)
                                            (Eccentricity _0)
                                            (RightAscensionOfAscendingNode _0)
                                            (Inclination _0)
                                            (ArgumentOfPeriapsis _0)
                                            (MeanAnomalyAtEpoch $ 0.9 *~ radian)
_bodyOrbit Tylo   = Just $ classical Jool   (SemiMajorAxis $ 68500000 *~ meter)
                                            (Eccentricity _0)
                                            (RightAscensionOfAscendingNode _0)
                                            (Inclination _0)
                                            (ArgumentOfPeriapsis _0)
                                            (MeanAnomalyAtEpoch $ 3.14 *~ radian)
_bodyOrbit Bop    = Just $ classical Jool   (SemiMajorAxis $ 128500000 *~ meter)
                                            (Eccentricity $ 0.24 *~ one)
                                            (RightAscensionOfAscendingNode $ 10 *~ degree)
                                            (Inclination $ 15 *~ degree)
                                            (ArgumentOfPeriapsis $ 25 *~ degree)
                                            (MeanAnomalyAtEpoch $ 0.9 *~ radian)
_bodyOrbit Pol    = Just $ classical Jool   (SemiMajorAxis $ 179890000 *~ meter)
                                            (Eccentricity $ 0.17 *~ one)
                                            (RightAscensionOfAscendingNode $ 2 *~ degree)
                                            (Inclination $ 4.25 *~ degree)
                                            (ArgumentOfPeriapsis $ 15 *~ degree)
                                            (MeanAnomalyAtEpoch $ 0.9 *~ radian)
_bodyOrbit Eeloo  = Just $ classical Kerbol (SemiMajorAxis $ 90118820000 *~ meter)
                                            (Eccentricity $ 0.26 *~ one)
                                            (RightAscensionOfAscendingNode $ 50 *~ degree)
                                            (Inclination $ 6.15 *~ degree)
                                            (ArgumentOfPeriapsis $ 260 *~ degree)
                                            (MeanAnomalyAtEpoch $ 3.14 *~ radian)

bodyOrbit :: Fold BodyId Orbit
bodyOrbit = folding _bodyOrbit

parentBodyId :: Fold Body BodyId
parentBodyId = bodyId . bodyOrbit . orbitBodyId

orbitBody :: Getter Orbit Body
orbitBody = orbitBodyId . fromBodyId

orbitMu :: Getter Orbit BodyGravitationalParam
orbitMu = orbitBody . bodyGravitationalParam

sma0 :: Orbit -> Length Double
sma0 orbit = quadrance (orbit ^. angularMomentum . _Wrapped') / (orbit ^. orbitMu . _Wrapped')

eccentricityM :: Orbit -> Dimensionless Double
eccentricityM orbit =  norm $ orbit ^. eccentricityVector . _Wrapped'

_eccentricity :: Orbit -> Eccentricity Double
_eccentricity orbit = Eccentricity $ eccentricityM orbit

-- TODO keep sma, except if sign (e - 1) changes, then what?
_setEccentricity :: Orbit -> Eccentricity Double -> Orbit
_setEccentricity orbit e = orbit & classicalIso . cOrbitEccentricity .~ e

eccentricity :: Lens' Orbit (Eccentricity Double)
eccentricity = lens _eccentricity _setEccentricity

_semiMajorAxis :: Orbit -> SemiMajorAxis
_semiMajorAxis orbit = SemiMajorAxis $ sma0 orbit / (_1 - (eccentricityM orbit ^ pos2))

_setSemiMajorAxis :: Orbit -> SemiMajorAxis -> Orbit
_setSemiMajorAxis orbit sma = orbit & classicalIso . cOrbitSemiMajorAxis .~ sma

semiMajorAxis :: Lens' Orbit SemiMajorAxis
semiMajorAxis = lens _semiMajorAxis _setSemiMajorAxis

_apoapsis :: Orbit -> Apoapsis
_apoapsis orbit = Apoapsis $ sma0 orbit / (_1 - eccentricityM orbit) - orbit ^. orbitBody . bodyRadius . _Wrapped'

_setApoapsis :: Orbit -> Apoapsis -> Orbit
_setApoapsis orbit ap = orbit & semiMajorAxis .~ SemiMajorAxis sma
                              & eccentricity .~ Eccentricity e
  where
    sma = orbit ^. semiMajorAxis . _Wrapped' + (ap ^. _Wrapped' - _apoapsis orbit ^. _Wrapped')
    e = (ap ^. _Wrapped' - _periapsis orbit ^. _Wrapped') / (_2 * sma)

apoapsis :: Lens' Orbit Apoapsis
apoapsis = lens _apoapsis _setApoapsis

_periapsis :: Orbit -> Periapsis
_periapsis orbit = Periapsis $ sma0 orbit / (_1 + eccentricityM orbit) - orbit ^. orbitBody . bodyRadius . _Wrapped'

_setPeriapsis :: Orbit -> Periapsis -> Orbit
_setPeriapsis orbit pe = orbit & semiMajorAxis .~ SemiMajorAxis sma
                               & eccentricity .~ Eccentricity e
  where
    sma = orbit ^. semiMajorAxis . _Wrapped' + (pe ^. _Wrapped' - _periapsis orbit ^. _Wrapped')
    e = (_apoapsis orbit ^. _Wrapped' - pe ^. _Wrapped') / (_2 * sma)

periapsis :: Lens' Orbit Periapsis
periapsis = lens _periapsis _setPeriapsis


_rightAscensionOfAscendingNode :: Orbit -> RightAscensionOfAscendingNode
_rightAscensionOfAscendingNode orbit = RightAscensionOfAscendingNode $
    atan2 (orbit ^. angularMomentum . _Wrapped' . _x) (negate $ orbit ^. angularMomentum . _Wrapped' . _y)

_setRightAscensionOfAscendingNode :: Orbit -> RightAscensionOfAscendingNode -> Orbit
_setRightAscensionOfAscendingNode orbit raan = orbit & classicalIso . cOrbitRightAscensionOfAscendingNode .~ raan

rightAscensionOfAscendingNode :: Lens' Orbit RightAscensionOfAscendingNode
rightAscensionOfAscendingNode = lens _rightAscensionOfAscendingNode _setRightAscensionOfAscendingNode

_inclination :: Orbit -> Inclination
_inclination orbit = Inclination $
    atan2 (norm $ orbit ^. angularMomentum . _Wrapped' . _xy) (orbit ^. angularMomentum . _Wrapped' . _z)

_setInclination :: Orbit -> Inclination -> Orbit
_setInclination orbit incl = orbit & classicalIso . cOrbitInclination .~ incl

inclination :: Lens' Orbit Inclination
inclination = lens _inclination _setInclination

_argumentOfPeriapsis :: Orbit -> ArgumentOfPeriapsis
_argumentOfPeriapsis orbit = ArgumentOfPeriapsis $ atan2 y x
  where
    y = quadrance (h ^. _xy) * (e ^. _z) - ((h ^. _xy) `dot` (e ^. _xy)) * (h ^. _z)
    x = norm h * (h ^. _x * e ^. _y - h ^. _y * e ^. _x)
    h = orbit ^. angularMomentum . _Wrapped'
    e = orbit ^. eccentricityVector . _Wrapped'

_setArgumentOfPeriapsis :: Orbit -> ArgumentOfPeriapsis -> Orbit
_setArgumentOfPeriapsis orbit argpe = orbit & classicalIso . cOrbitArgumentOfPeriapsis .~ argpe

argumentOfPeriapsis :: Lens' Orbit ArgumentOfPeriapsis
argumentOfPeriapsis = lens _argumentOfPeriapsis _setArgumentOfPeriapsis

_orbitalPeriod :: Orbit -> OrbitalPeriod
_orbitalPeriod orbit = OrbitalPeriod $ tau * sqrt (sma ^ pos3 / orbit ^. orbitMu . _Wrapped')
  where
    SemiMajorAxis sma = _semiMajorAxis orbit

orbitalPeriod :: Getter Orbit OrbitalPeriod
orbitalPeriod = to _orbitalPeriod

_specificEnergy :: Orbit -> OrbitSpecificEnergy
_specificEnergy orbit = OrbitSpecificEnergy . negate $ orbit ^. orbitMu . _Wrapped' / (_2 * sma)
  where
    SemiMajorAxis sma = _semiMajorAxis orbit

specificEnergy :: Getter Orbit OrbitSpecificEnergy
specificEnergy = to _specificEnergy
