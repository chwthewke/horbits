{-# LANGUAGE TemplateHaskell #-}

module Horbits.Orbit.Geometry where

import           Control.Lens                         hiding ((*~), _1)
import           Horbits.DimLin
import           Horbits.Orbit
import           Numeric.Units.Dimensional.TF.Prelude
import           Prelude                              hiding (negate, sqrt, (*), (/))

data CenterEllipse = CenterEllipse { _center              :: Length (V3 Double)
                                   , _semiMajorAxisVector :: Length (V3 Double)
                                   , _semiMinorAxisVector :: Length (V3 Double)
                                   }


makeLenses ''CenterEllipse

_toCentralOrbit :: Orbit -> CenterEllipse
_toCentralOrbit orbit = CenterEllipse (negate $ orbit ^. eccentricity * smaa *^ a) (smaa *^ a) (smia *^ b)
  where
    (a, b) = semiAxes orbit
    smaa = orbit ^. semiMajorAxis
    smia = sqrt $ orbit ^. semiMajorAxis * orbit ^. semiLatusRectum

centralOrbit :: Getting CenterEllipse Orbit CenterEllipse
centralOrbit = to _toCentralOrbit

semiAxes :: Orbit -> (Dimensionless (V3 Double), Dimensionless (V3 Double))
semiAxes orbit = if dimNearZero _1 (orbit ^. eccentricity)
                 then circularSemiAxes orbit
                 else eccentricSemiAxes orbit

eccentricSemiAxes :: Orbit -> (Dimensionless (V3 Double), Dimensionless (V3 Double))
eccentricSemiAxes orbit = (normalize (orbit ^. eccentricityVector), b)
  where
    a = normalize (orbit ^. eccentricityVector)
    b = normalize ((orbit ^. angularMomentum) `cross` a)

circularSemiAxes :: Orbit -> (Dimensionless (V3 Double), Dimensionless (V3 Double))
circularSemiAxes orbit = (a, b)
  where
    h = orbit ^. angularMomentum
    c = if dimNearZero (norm h) (h ^. _z)
        then v3 _0 _0 _1
        else v3 _1 _0 _0
    a = normalize (h `cross` c)
    b = normalize (h `cross` a)
