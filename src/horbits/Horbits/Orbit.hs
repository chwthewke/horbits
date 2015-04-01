{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}

-- TODO split, move into Horbits.Orbit, re-export

module Horbits.Orbit
    (Orbit(Orbit), orbitBodyId, parentBodyId, angularMomentum, eccentricityVector, meanAnomalyAtEpoch, classical,
    bodyOrbit, orbitBody, orbitMu, eccentricity, semiMajorAxis, semiLatusRectum, apoapsisHeight, apoapsis,
    periapsisHeight, periapsis, rightAscensionOfAscendingNode, inclination, argumentOfPeriapsis, orbitalPeriod,
    specificEnergy, bodySolarDay, orbitalVelocity, OrbitalVelocity(OrbitalVelocity), orbitalVelocityMin,
    orbitalVelocityMax)
 where

import           Control.Applicative
import           Control.Lens                         hiding ((*~), _1, _2)
import           Control.Monad                        (mfilter)
import           Horbits.Body
import           Horbits.DimLin
import           Horbits.Types
import           Numeric.Units.Dimensional.TF.Prelude hiding (atan2, map)
import           Prelude                              hiding (atan2, map, negate, pi, sqrt, (*), (+), (-), (/), (^))


data OrbitalVelocity = OrbitalVelocity { _orbitalVelocityMin :: Velocity Double
                                       , _orbitalVelocityMax :: Velocity Double
                                       } deriving (Show, Eq)
makeLenses ''OrbitalVelocity


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
_bodyOrbit Sun    = Nothing
_bodyOrbit Kerbin = Just $ classical Sun    (13599840256 *~ meter)
                                            (0.000000 *~ one)
                                            (0.00 *~ degree)
                                            (0.00 *~ degree)
                                            (0.00 *~ degree)
                                            (3.14 *~ radian)
_bodyOrbit Mun    = Just $ classical Kerbin (12000000 *~ meter)
                                            (0.000000 *~ one)
                                            (0.00 *~ degree)
                                            (0.00 *~ degree)
                                            (0.00 *~ degree)
                                            (1.70 *~ radian)
_bodyOrbit Minmus = Just $ classical Kerbin (47000000 *~ meter)
                                            (0.000000 *~ one)
                                            (78.00 *~ degree)
                                            (6.00 *~ degree)
                                            (38.00 *~ degree)
                                            (0.90 *~ radian)
_bodyOrbit Moho   = Just $ classical Sun    (5263138304 *~ meter)
                                            (0.200000 *~ one)
                                            (70.00 *~ degree)
                                            (7.00 *~ degree)
                                            (15.00 *~ degree)
                                            (3.14 *~ radian)
_bodyOrbit Eve    = Just $ classical Sun    (9832684544 *~ meter)
                                            (0.010000 *~ one)
                                            (15.00 *~ degree)
                                            (2.10 *~ degree)
                                            (0.00 *~ degree)
                                            (3.14 *~ radian)
_bodyOrbit Duna   = Just $ classical Sun    (20726155264 *~ meter)
                                            (0.051000 *~ one)
                                            (135.50 *~ degree)
                                            (0.06 *~ degree)
                                            (0.00 *~ degree)
                                            (3.14 *~ radian)
_bodyOrbit Ike    = Just $ classical Duna   (3200000 *~ meter)
                                            (0.030000 *~ one)
                                            (0.00 *~ degree)
                                            (0.20 *~ degree)
                                            (0.00 *~ degree)
                                            (1.70 *~ radian)
_bodyOrbit Jool   = Just $ classical Sun    (68773560320 *~ meter)
                                            (0.050000 *~ one)
                                            (52.00 *~ degree)
                                            (1.30 *~ degree)
                                            (0.00 *~ degree)
                                            (0.10 *~ radian)
_bodyOrbit Laythe = Just $ classical Jool   (27184000 *~ meter)
                                            (0.000000 *~ one)
                                            (0.00 *~ degree)
                                            (0.00 *~ degree)
                                            (0.00 *~ degree)
                                            (3.14 *~ radian)
_bodyOrbit Vall   = Just $ classical Jool   (43152000 *~ meter)
                                            (0.000000 *~ one)
                                            (0.00 *~ degree)
                                            (0.00 *~ degree)
                                            (0.00 *~ degree)
                                            (0.90 *~ radian)
_bodyOrbit Bop    = Just $ classical Jool   (128500000 *~ meter)
                                            (0.235000 *~ one)
                                            (10.00 *~ degree)
                                            (15.00 *~ degree)
                                            (25.00 *~ degree)
                                            (0.90 *~ radian)
_bodyOrbit Tylo   = Just $ classical Jool   (68500000 *~ meter)
                                            (0.000000 *~ one)
                                            (0.00 *~ degree)
                                            (0.03 *~ degree)
                                            (0.00 *~ degree)
                                            (3.14 *~ radian)
_bodyOrbit Gilly  = Just $ classical Eve    (31500000 *~ meter)
                                            (0.550000 *~ one)
                                            (80.00 *~ degree)
                                            (12.00 *~ degree)
                                            (10.00 *~ degree)
                                            (0.90 *~ radian)
_bodyOrbit Pol    = Just $ classical Jool   (179890000 *~ meter)
                                            (0.170850 *~ one)
                                            (2.00 *~ degree)
                                            (4.25 *~ degree)
                                            (15.00 *~ degree)
                                            (0.90 *~ radian)
_bodyOrbit Dres   = Just $ classical Sun    (40839348203 *~ meter)
                                            (0.145000 *~ one)
                                            (280.00 *~ degree)
                                            (5.00 *~ degree)
                                            (90.00 *~ degree)
                                            (3.14 *~ radian)
_bodyOrbit Eeloo  = Just $ classical Sarnus (19105978 *~ meter)
                                            (0.003400 *~ one)
                                            (55.00 *~ degree)
                                            (2.30 *~ degree)
                                            (260.00 *~ degree)
                                            (3.14 *~ radian)
_bodyOrbit Sarnus = Just $ classical Sun    (125798522368 *~ meter)
                                            (0.053400 *~ one)
                                            (184.00 *~ degree)
                                            (2.02 *~ degree)
                                            (0.00 *~ degree)
                                            (2.88 *~ radian)
_bodyOrbit Urlum  = Just $ classical Sun    (254317012787 *~ meter)
                                            (0.045215 *~ one)
                                            (61.00 *~ degree)
                                            (0.64 *~ degree)
                                            (0.00 *~ degree)
                                            (5.60 *~ radian)
_bodyOrbit Neidon = Just $ classical Sun    (409355191706 *~ meter)
                                            (0.012757 *~ one)
                                            (259.00 *~ degree)
                                            (1.27 *~ degree)
                                            (0.00 *~ degree)
                                            (2.27 *~ radian)
_bodyOrbit Hale   = Just $ classical Sarnus (10488231 *~ meter)
                                            (0.000000 *~ one)
                                            (55.00 *~ degree)
                                            (1.00 *~ degree)
                                            (0.00 *~ degree)
                                            (0.00 *~ radian)
_bodyOrbit Ovok   = Just $ classical Sarnus (12169413 *~ meter)
                                            (0.010000 *~ one)
                                            (55.00 *~ degree)
                                            (1.50 *~ degree)
                                            (0.00 *~ degree)
                                            (1.72 *~ radian)
_bodyOrbit Slate  = Just $ classical Sarnus (42592946 *~ meter)
                                            (0.040000 *~ one)
                                            (55.00 *~ degree)
                                            (2.30 *~ degree)
                                            (0.00 *~ degree)
                                            (1.10 *~ radian)
_bodyOrbit Plock  = Just $ classical Sun    (535833706086 *~ meter)
                                            (0.260000 *~ one)
                                            (260.00 *~ degree)
                                            (6.15 *~ degree)
                                            (50.00 *~ degree)
                                            (0.00 *~ radian)
_bodyOrbit Tekto  = Just $ classical Sarnus (97355304 *~ meter)
                                            (0.028000 *~ one)
                                            (55.00 *~ degree)
                                            (9.40 *~ degree)
                                            (0.00 *~ degree)
                                            (2.10 *~ radian)
_bodyOrbit Polta  = Just $ classical Urlum  (11727895 *~ meter)
                                            (0.001500 *~ one)
                                            (40.00 *~ degree)
                                            (2.45 *~ degree)
                                            (60.00 *~ degree)
                                            (1.52 *~ radian)
_bodyOrbit Priax  = Just $ classical Urlum  (11727895 *~ meter)
                                            (0.001500 *~ one)
                                            (40.00 *~ degree)
                                            (2.50 *~ degree)
                                            (0.00 *~ degree)
                                            (1.52 *~ radian)
_bodyOrbit Wal    = Just $ classical Urlum  (33776834 *~ meter)
                                            (0.023000 *~ one)
                                            (40.00 *~ degree)
                                            (1.90 *~ degree)
                                            (0.00 *~ degree)
                                            (2.96 *~ radian)

bodyOrbit :: Fold BodyId Orbit
bodyOrbit = folding _bodyOrbit

parentBodyId :: Fold BodyId BodyId
parentBodyId = bodyOrbit . orbitBodyId

orbitBody :: Getter Orbit Body
orbitBody = orbitBodyId . fromBodyId

orbitMu :: Getter Orbit (GravitationalParameter Double)
orbitMu = orbitBody . bodyGravitationalParam

_semiLatusRectum :: Orbit -> Length Double
_semiLatusRectum = do
    h <- view angularMomentum
    mu <- view orbitMu
    return $ quadrance h / mu

semiLatusRectum :: Getter Orbit (Length Double)
semiLatusRectum = to _semiLatusRectum


_eccentricity :: Orbit -> Dimensionless Double
_eccentricity = do
    e <- view eccentricityVector
    return $ norm e

-- TODO keep sma, except if sign (e - 1) changes, then what?
_setEccentricity :: Orbit -> Dimensionless Double -> Orbit
_setEccentricity = flip . set $ classicalIso . cOrbitEccentricity


eccentricity :: Lens' Orbit (Dimensionless Double)
eccentricity = lens _eccentricity _setEccentricity

_semiMajorAxis :: Orbit -> Length Double
_semiMajorAxis = do
    p <- view semiLatusRectum
    e <- view eccentricity
    return $ p / (_1 - e ^ pos2)

_setSemiMajorAxis :: Orbit -> Length Double -> Orbit
_setSemiMajorAxis = flip . set $ classicalIso . cOrbitSemiMajorAxis


semiMajorAxis :: Lens' Orbit (Length Double)
semiMajorAxis = lens _semiMajorAxis _setSemiMajorAxis


_apoapsisHeight :: Orbit -> Length Double
_apoapsisHeight = do
    p <- view semiLatusRectum
    e <- view eccentricity
    return $ p / (_1 - e)

_setApoapsisHeight :: Orbit -> Length Double -> Orbit
_setApoapsisHeight orbit ap = orbit & semiMajorAxis %~ (+ dAp)
                                    & eccentricity .~ e
  where
    dAp = ap - _apoapsisHeight orbit
    e = ap / (orbit ^. semiMajorAxis + dAp) -_1

apoapsisHeight :: Lens' Orbit (Length Double)
apoapsisHeight = lens _apoapsisHeight _setApoapsisHeight

_apoapsis :: Orbit -> Length Double
_apoapsis orbit = orbit ^. apoapsisHeight - orbit ^. orbitBody . bodyRadius

_setApoapsis :: Orbit -> Length Double -> Orbit
_setApoapsis orbit apo = orbit & apoapsisHeight .~ apo + orbit ^. orbitBody . bodyRadius

apoapsis :: Lens' Orbit (Length Double)
apoapsis = lens _apoapsis _setApoapsis

_periapsisHeight :: Orbit -> Length Double
_periapsisHeight = do
    p <- view semiLatusRectum
    e <- view eccentricity
    return $ p / (_1 + e)

_setPeriapsisHeight :: Orbit -> Length Double -> Orbit
_setPeriapsisHeight orbit pe = orbit & semiMajorAxis %~ (+ dPe)
                                     & eccentricity .~ e
  where
    dPe = pe - _periapsisHeight orbit
    e = _1 - pe / (orbit ^. semiMajorAxis + dPe)

periapsisHeight :: Lens' Orbit (Length Double)
periapsisHeight = lens _periapsisHeight _setPeriapsisHeight

_periapsis :: Orbit -> Length Double
_periapsis orbit = orbit ^. periapsisHeight - orbit ^. orbitBody . bodyRadius

_setPeriapsis :: Orbit -> Length Double -> Orbit
_setPeriapsis orbit pe = orbit & periapsisHeight .~ pe + orbit ^. orbitBody . bodyRadius


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

orbitalVelocity :: Getter Orbit OrbitalVelocity
orbitalVelocity = to $ do
    h <- view angularMomentum
    mu <- view orbitMu
    e <- view eccentricity
    let v0 = mu / norm h
    return $ OrbitalVelocity (v0 * (_1 - e)) (v0 * (_1 + e))

kerbolOrbit :: Maybe Orbit -> Maybe Orbit
kerbolOrbit = mfilter ((== Sun) <$> view orbitBodyId)

_bodySolarDay :: Body -> Maybe Orbit -> Maybe (Time Double)
_bodySolarDay b = fmap solarDay
  where
    siderealDay = b ^. bodySiderealRotationPeriod
    siderealYear o = o ^. orbitalPeriod
    solarDay o = siderealDay / (_1 - siderealDay / siderealYear o)

bodySolarDay :: Fold Body (Time Double)
bodySolarDay = folding $ do
    orbit <- views (bodyId . pre bodyOrbit) kerbolOrbit
    flip _bodySolarDay orbit