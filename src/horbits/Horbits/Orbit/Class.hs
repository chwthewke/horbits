{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Horbits.Orbit.Class
    (VectorOrbit(VectorOrbit), HasVectorOrbit(..),
    ClassicalOrbit(ClassicalOrbit), HasClassicalOrbit(..),
    OrbitClass,
    orbitBody, orbitMu, orbitSemiLatusRectum)
  where

import           Control.Applicative
import           Control.Lens                         hiding (_1, _2)
import           Horbits.Body
import           Horbits.DimLin
import           Horbits.Types
import           Numeric.Units.Dimensional.TF.Prelude hiding (atan2)
import           Prelude                              hiding (atan2, negate, sqrt, (*), (-), (/), (^))

-- Definitions and classes

data VectorOrbit = VectorOrbit { _orbitBodyId             :: BodyId
                               , _orbitAngularMomentum    :: SpecificAngularMomentum (V3 Double)
                               , _orbitEccentricityVector :: Dimensionless (V3 Double)
                               , _orbitMeanAnomalyAtEpoch :: Dimensionless Double
                               } deriving (Show, Eq)

makeClassy ''VectorOrbit

data ClassicalOrbit = ClassicalOrbit { _cOrbitBodyId                       :: BodyId
                                     , _orbitSemiMajorAxis                 :: Length Double
                                     , _orbitEccentricity                  :: Dimensionless Double
                                     , _orbitRightAscensionOfAscendingNode :: Dimensionless Double
                                     , _orbitInclination                   :: Dimensionless Double
                                     , _orbitArgumentOfPeriapsis           :: Dimensionless Double
                                     , _cOrbitMeanAnomalyAtEpoch           :: Dimensionless Double
                                     } deriving (Show, Eq)

makeClassyFor
    "HasClassicalOrbit"
    "classicalOrbit"
    [("_orbitSemiMajorAxis", "orbitSemiMajorAxis"),
     ("_orbitEccentricity", "orbitEccentricity"),
     ("_orbitRightAscensionOfAscendingNode", "orbitRightAscensionOfAscendingNode"),
     ("_orbitInclination", "orbitInclination"),
     ("_orbitArgumentOfPeriapsis", "orbitArgumentOfPeriapsis")]
    ''ClassicalOrbit

-- Class equivalence

instance HasClassicalOrbit VectorOrbit where
  classicalOrbit = classicalIso

instance HasVectorOrbit ClassicalOrbit where
  vectorOrbit = from classicalIso

orbitToClassical :: VectorOrbit -> ClassicalOrbit
orbitToClassical = ClassicalOrbit <$> _orbitBodyId
                                  <*> _semiMajorAxis
                                  <*> _eccentricity
                                  <*> _rightAscensionOfAscendingNode
                                  <*> _inclination
                                  <*> _argumentOfPeriapsis
                                  <*> _orbitMeanAnomalyAtEpoch

classicalToOrbit :: ClassicalOrbit -> VectorOrbit
classicalToOrbit = classical <$> _cOrbitBodyId
                             <*> _orbitSemiMajorAxis
                             <*> _orbitEccentricity
                             <*> _orbitRightAscensionOfAscendingNode
                             <*> _orbitInclination
                             <*> _orbitArgumentOfPeriapsis
                             <*> _cOrbitMeanAnomalyAtEpoch
  where
    classical b a e lan i p = VectorOrbit b (rotate rot $ v3 _0 _0 h) (rotate rot $ v3 e _0 _0)
      where
        h = sqrt $ a * b ^. fromBodyId . bodyGravitationalParam * (_1 - e ^ pos2)
        rot = rotZ lan * rotX i * rotZ p

classicalIso :: Iso' VectorOrbit ClassicalOrbit
classicalIso = iso orbitToClassical classicalToOrbit

type OrbitClass t = (HasVectorOrbit t, HasClassicalOrbit t)

-- Orbit class

-- Properties


orbitBody :: HasVectorOrbit t => Getter t Body
orbitBody = orbitBodyId . fromBodyId

-- MU

orbitMu :: HasVectorOrbit t => Getter t (GravitationalParameter Double)
orbitMu = orbitBody . bodyGravitationalParam

-- SLR

_semiLatusRectum :: HasVectorOrbit t => t -> Length Double
_semiLatusRectum = do
    h <- view orbitAngularMomentum
    mu <- view orbitMu
    return $ quadrance h / mu

orbitSemiLatusRectum :: OrbitClass t => Getter t (Length Double)
orbitSemiLatusRectum = to _semiLatusRectum

-- ECC

_eccentricity :: HasVectorOrbit t => t -> Dimensionless Double
_eccentricity = do
    e <- view orbitEccentricityVector
    return $ norm e

-- SMA

_semiMajorAxis :: HasVectorOrbit t => t -> Length Double
_semiMajorAxis = do
    p <- _semiLatusRectum
    e <- _eccentricity
    return $ p / (_1 - e ^ pos2)

-- RAAN

_rightAscensionOfAscendingNode :: HasVectorOrbit t => t -> Dimensionless Double
_rightAscensionOfAscendingNode orbit =
    atan2 (orbit ^. orbitAngularMomentum . _x) (negate $ orbit ^. orbitAngularMomentum . _y)


-- INCL

_inclination :: HasVectorOrbit t => t -> Dimensionless Double
_inclination orbit =
    atan2 (norm $ orbit ^. orbitAngularMomentum . _xy) (orbit ^. orbitAngularMomentum . _z)


-- ARG. PE

_argumentOfPeriapsis :: HasVectorOrbit t => t -> Dimensionless Double
_argumentOfPeriapsis orbit = atan2 y x
  where
    y = quadrance (h ^. _xy) * (e ^. _z) - ((h ^. _xy) `dot` (e ^. _xy)) * (h ^. _z)
    x = norm h * (h ^. _x * e ^. _y - h ^. _y * e ^. _x)
    h = orbit ^. orbitAngularMomentum
    e = orbit ^. orbitEccentricityVector
