{-# LANGUAGE TemplateHaskell #-}

module Horbits.Orbit.Geometry where

import           Control.Lens                         hiding ((*~), _1)
import           Horbits.DimLin
import           Horbits.Orbit.Class
import           Horbits.Orbit.Properties
import           Numeric.Units.Dimensional.TF.Prelude
import           Prelude                              hiding (negate, sqrt, (*), (/))

data CenterEllipse = CenterEllipse { _center              :: Length (V3 Double)
                                   , _semiMajorAxisVector :: Length (V3 Double)
                                   , _semiMinorAxisVector :: Length (V3 Double)
                                   }


makeLenses ''CenterEllipse

centralOrbit :: OrbitClass t => Getter t CenterEllipse
centralOrbit = to $ do
    (a, b) <- semiAxes
    smaa <- view orbitSemiMajorAxis
    smia <- view orbitSemiMinorAxis
    e <- view orbitEccentricity
    let smaaVector = smaa *^ a
    let smiaVector = smia *^ b
    let c = negate $ e *^ smaaVector
    return $ CenterEllipse c smaaVector smiaVector

semiAxes :: OrbitClass t => t -> (Dimensionless (V3 Double), Dimensionless (V3 Double))
semiAxes orbit = if dimNearZero _1 (orbit ^. orbitEccentricity)
                 then circularSemiAxes orbit
                 else eccentricSemiAxes orbit

eccentricSemiAxes :: OrbitClass t => t -> (Dimensionless (V3 Double), Dimensionless (V3 Double))
eccentricSemiAxes = do
    e <- view orbitEccentricityVector
    h <- view orbitAngularMomentumVector
    let a = normalize e
    let b = normalize $ h `cross` a
    return (a, b)

-- TODO for this use, can we make good use of the existing raan/arg.pe?
circularSemiAxes :: OrbitClass t => t -> (Dimensionless (V3 Double), Dimensionless (V3 Double))
circularSemiAxes = do
    h <- view orbitAngularMomentumVector
    let c = if dimNearZero (norm h) (h ^. _z)
                then v3 _0 _0 _1
                else v3 _1 _0 _0
    let a = normalize $ h `cross` c
    let b = normalize $ h `cross` a
    return (a, b)
