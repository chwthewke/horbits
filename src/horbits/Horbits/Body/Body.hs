{-# LANGUAGE TemplateHaskell #-}

module Horbits.Body.Body(Body(Body),
    bodyId, bodyName, bodyGravitationalParam, bodyRadius, bodySiderealRotationPeriod, bodyAtmosphere,
    bodySphereOfInfluence, bodySurfaceArea, bodyMass, bodyDensity, bodySurfaceGravity, bodyEscapeVelocity,
    bodySiderealRotationVelocity, bodySynchronousOrbitAltitude) where

import           Control.Lens                         hiding ((*~), _2, _3, _4)
import           Numeric.Units.Dimensional.TF.Prelude
import           Prelude                              hiding (pi, sqrt, (*), (-), (/), (^))

import           Horbits.Body.Atmosphere
import           Horbits.Body.Id

data Body = Body { _bodyId                     :: BodyId
                 , _bodyName                   :: String
                 , _bodyGravitationalParam     :: GravitationalParameter Double
                 , _bodyRadius                 :: Length Double
                 , _bodySiderealRotationPeriod :: Time Double
                 , _bodySoI                    :: Maybe (Length Double)
                 , _bodyAtmo                   :: Maybe Atmosphere
                 } deriving (Show, Eq)

makeLenses ''Body

gravitationalConstant :: Quantity (Div DGravitationalParameter DMass) Double
gravitationalConstant =  6.67384e-11 *~ ((meter ^ pos3) / kilo gram / (second ^ pos2))

-- Derived properties
bodyAtmosphere :: Fold Body Atmosphere
bodyAtmosphere = bodyAtmo . traverse

bodySphereOfInfluence :: Fold Body (Length Double)
bodySphereOfInfluence = bodySoI . traverse

bodySurfaceArea :: Getter Body (Area Double)
bodySurfaceArea = bodyRadius . to (\r -> _4 * pi * r * r)

bodyMass :: Getter Body (Mass Double)
bodyMass = bodyGravitationalParam . to (/ gravitationalConstant)

bodyDensity :: Getter Body (Density Double)
bodyDensity = to $ do
    mass <- view bodyMass
    radius <- view bodyRadius
    return $ mass / (_4 / _3 * pi * radius ^ pos3)

bodySurfaceGravity :: Getter Body (Acceleration Double)
bodySurfaceGravity = to $ do
    mu <- view bodyGravitationalParam
    radius <- view bodyRadius
    return $ mu / (radius ^ pos2)

bodyEscapeVelocity :: Getter Body (Velocity Double)
bodyEscapeVelocity = to $ do
    mu <- view bodyGravitationalParam
    radius <- view bodyRadius
    return $ sqrt (_2 * mu / radius)

bodySiderealRotationVelocity :: Getter Body (Velocity Double)
bodySiderealRotationVelocity = to $ do
    period <- view bodySiderealRotationPeriod
    r <- view bodyRadius
    return $ _2 * pi * r / period

bodySynchronousOrbitAltitude :: Getter Body (Length Double)
bodySynchronousOrbitAltitude = to $ do
    period <- view bodySiderealRotationPeriod
    mu <- view bodyGravitationalParam
    radius <- view bodyRadius
    return $ cbrt (mu * (period / (_2 * pi)) ^ pos2) - radius
