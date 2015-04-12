{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Horbits.Orbit.Geometry(centralOrbit, semiAxes, center, semiMajorAxisVector, semiMinorAxisVector) where

import           Control.Lens                hiding ((*~), _1, _2)

import           Horbits.Dimensional.Prelude
import           Horbits.Orbit.Class
import           Horbits.Orbit.Properties

-- TODO names! also, direct lenses on Orbit?
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
semiAxes orbit = if nearZero (orbit ^. orbitEccentricity)
                 then circularSemiAxes orbit
                 else eccentricSemiAxes orbit

eccentricSemiAxes :: OrbitClass t => t -> (Dimensionless (V3 Double), Dimensionless (V3 Double))
eccentricSemiAxes = do
    e <- view orbitEccentricityVector
    h <- view orbitAngularMomentumVector
    let a = normalize e
    let b = normalize $ h `cross` a
    return (a, b)

-- TODO for this use, can we make good use of the existing raan/arg.pe? -- yes but tests would be great
circularSemiAxes :: OrbitClass t => t -> (Dimensionless (V3 Double), Dimensionless (V3 Double))
circularSemiAxes = do
    h <- view orbitAngularMomentumVector
    raan <- view orbitRightAscensionOfAscendingNode
    argp <- view orbitArgumentOfPeriapsis
    let c = v3 (cos raan) (sin raan) _0
    let a = rotate (axisAngle h (negate argp)) c
    let b = rotate (axisAngle h (pi / _2)) a
    return (a, b)
