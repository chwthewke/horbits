{-# LANGUAGE TemplateHaskell #-}

module Horbits.Body (Atmosphere, Body, BodyId(..), Rgb8Color(Rgb8Color), gravitationalConstant, atmosphereHeight,
    atmosphereScaleHeight, atmosphericPressure, bodyId, bodyName, bodyGravitationalParam, bodyRadius,
    bodySiderealRotationPeriod, bodySphereOfInfluence, bodyAtmosphere, bodyAtmosphereHeight, bodyAtmosphereScaleHeight,
    bodyAtmosphericPressure, bodySurfaceArea, bodyMass, bodyDensity, bodySurfaceGravity, bodyEscapeVelocity,
    bodySiderealRotationVelocity, bodySynchronousOrbitAltitude, bodyUiColor, rgbColorR, rgbColorG, rgbColorB,
    getBody, fromBodyId)
  where

import           Control.Lens                         hiding ((*~), _2, _3, _4)
import           Data.Word                            (Word8)
import           Numeric.Units.Dimensional.TF.Prelude
import           Prelude                              hiding (pi, sqrt, (*), (-), (/), (^))


gravitationalConstant :: Quantity (Div DGravitationalParameter DMass) Double
gravitationalConstant =  6.67384e-11 *~ ((meter ^ pos3) / kilo gram / (second ^ pos2))

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
    deriving (Bounded, Enum, Show, Eq)

data Atmosphere = Atmosphere { _atmosphereHeight      :: Length Double
                             , _atmosphericPressure   :: Pressure Double
                             , _atmosphereScaleHeight :: Length Double
                             } deriving (Show, Eq)
makeLenses ''Atmosphere

data Body = Body { _bodyId                     :: BodyId
                 , _bodyName                   :: String
                 , _bodyGravitationalParam     :: GravitationalParameter Double
                 , _bodyRadius                 :: Length Double
                 , _bodySiderealRotationPeriod :: Time Double
                 , _bodySoI                    :: Maybe (Length Double)
                 , _bodyAtmosphere             :: Maybe Atmosphere
                 } deriving (Show, Eq)

makeLenses ''Body

data Rgb8Color = Rgb8Color { _rgbColorR :: Word8
                           , _rgbColorG :: Word8
                           , _rgbColorB :: Word8
                           }

makeLenses ''Rgb8Color

-- Derived properties
bodySphereOfInfluence :: Fold Body (Length Double)
bodySphereOfInfluence = bodySoI . traverse

bodyAtmosphereHeight :: Getter Body (Maybe (Length Double))
bodyAtmosphereHeight = pre (bodyAtmosphere . traverse . atmosphereHeight)

bodyAtmosphericPressure :: Getter Body (Maybe (Pressure Double))
bodyAtmosphericPressure = pre (bodyAtmosphere . traverse . atmosphericPressure)

bodyAtmosphereScaleHeight :: Getter Body (Maybe (Length Double))
bodyAtmosphereScaleHeight = pre (bodyAtmosphere . traverse . atmosphereScaleHeight)

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

bodyUiColor :: Fold BodyId Rgb8Color
bodyUiColor = to getColor . traverse

-- Data
getBody :: BodyId -> Body
getBody bId = Body bId (show bId) mu r t soi atm
  where
    (mu, soi) = getOrbitalAttrs bId
    (r, t) = getPhysicalAttrs bId
    atm = getAtmosphere bId


fromBodyId :: Getter BodyId Body
fromBodyId = to getBody

mkAtmosphere :: Double -> Double -> Double -> Maybe Atmosphere
mkAtmosphere h p s = Just $ Atmosphere (h *~ meter) (p *~ kilo pascal) (s *~ meter)

getAtmosphere :: BodyId -> Maybe Atmosphere
getAtmosphere Eve = mkAtmosphere 96708.574 506.625 7000
getAtmosphere Kerbin = mkAtmosphere 69077.53 101.325 5000
getAtmosphere Duna = mkAtmosphere 41446.532 20.2650 3000
getAtmosphere Jool = mkAtmosphere 138155.11 1519.88 10000
getAtmosphere Laythe = mkAtmosphere 55262.042 81.0600 4000
getAtmosphere _ = Nothing

mkPhysicalAttrs :: Double -> Double -> (Length Double, Time Double)
mkPhysicalAttrs r t = (r *~ meter, t *~ second)

getPhysicalAttrs :: BodyId -> (Length Double, Time Double)
getPhysicalAttrs Kerbol = mkPhysicalAttrs 261600000 432000
getPhysicalAttrs Moho = mkPhysicalAttrs 250000 1210000
getPhysicalAttrs Eve = mkPhysicalAttrs 700000 80500
getPhysicalAttrs Gilly = mkPhysicalAttrs 13000 28255
getPhysicalAttrs Kerbin = mkPhysicalAttrs 600000 21600
getPhysicalAttrs Mun = mkPhysicalAttrs 200000 138984.38
getPhysicalAttrs Minmus = mkPhysicalAttrs 60000 40400
getPhysicalAttrs Duna = mkPhysicalAttrs 320000 65517.859
getPhysicalAttrs Ike = mkPhysicalAttrs 130000 65517.862
getPhysicalAttrs Dres = mkPhysicalAttrs 138000 34800
getPhysicalAttrs Jool = mkPhysicalAttrs 6000000 36000
getPhysicalAttrs Laythe = mkPhysicalAttrs 500000 52980.879
getPhysicalAttrs Vall = mkPhysicalAttrs 300000 105962.09
getPhysicalAttrs Tylo = mkPhysicalAttrs 600000 211926.36
getPhysicalAttrs Bop = mkPhysicalAttrs 65000 544507.40
getPhysicalAttrs Pol = mkPhysicalAttrs 44000 901902.62
getPhysicalAttrs Eeloo = mkPhysicalAttrs 210000 19460

mkOrbitalAttrs :: Double -> Double -> (GravitationalParameter Double, Maybe (Length Double))
mkOrbitalAttrs mu r = (mu *~ (meter ^ pos3 / second ^ pos2), Just $ r *~ meter)

getOrbitalAttrs :: BodyId -> (GravitationalParameter Double, Maybe (Length Double))
getOrbitalAttrs Kerbol = (1.1723328e18 *~ (meter ^ pos3 / second ^ pos2), Nothing)
getOrbitalAttrs Moho = mkOrbitalAttrs 1.6860938e11 9.6466630e6
getOrbitalAttrs Eve = mkOrbitalAttrs 8.1717302e12 8.5109365e7
getOrbitalAttrs Gilly = mkOrbitalAttrs 8.2894498e6 1.2612327e5
getOrbitalAttrs Kerbin = mkOrbitalAttrs 3.5316e12 8.4159286e7
getOrbitalAttrs Mun = mkOrbitalAttrs 6.5138398e10 2.4295591e6
getOrbitalAttrs Minmus = mkOrbitalAttrs 1.7658000e9 2.2474284e6
getOrbitalAttrs Duna = mkOrbitalAttrs 3.0136321e11 4.7921949e7
getOrbitalAttrs Ike = mkOrbitalAttrs 1.8568369e10 1.0495989e6
getOrbitalAttrs Dres = mkOrbitalAttrs 2.1484489e10 3.2832840e7
getOrbitalAttrs Jool = mkOrbitalAttrs 2.8252800e14 2.4559852e9
getOrbitalAttrs Laythe = mkOrbitalAttrs 1.9620000e12 3.7236458e6
getOrbitalAttrs Vall = mkOrbitalAttrs 2.0748150e11 2.4064014e6
getOrbitalAttrs Tylo = mkOrbitalAttrs 2.8252800e12 1.0856518e7
getOrbitalAttrs Bop = mkOrbitalAttrs 2.4868349e9 1.2210609e6
getOrbitalAttrs Pol = mkOrbitalAttrs 7.2170208e8 1.0421389e6
getOrbitalAttrs Eeloo = mkOrbitalAttrs 7.4410815e10 1.1908294e8

getColor :: BodyId -> Maybe Rgb8Color
getColor Kerbol = Nothing
getColor Kerbin = Just $ Rgb8Color 0x7d 0xb7 0xb0
getColor Mun    = Just $ Rgb8Color 0x85 0x8a 0x9a
getColor Minmus = Just $ Rgb8Color 0x48 0x3a 0x50
getColor Moho   = Just $ Rgb8Color 0x78 0x5c 0x45
getColor Eve    = Just $ Rgb8Color 0x6d 0x20 0xe5
getColor Duna   = Just $ Rgb8Color 0xa4 0x3f 0x28
getColor Ike    = Just $ Rgb8Color 0x85 0x8a 0x9a
getColor Jool   = Just $ Rgb8Color 0x54 0x85 0x13
getColor Laythe = Just $ Rgb8Color 0x22 0x2b 0x4e
getColor Vall   = Just $ Rgb8Color 0x6f 0x9c 0xb5
getColor Bop    = Just $ Rgb8Color 0x5d 0x50 0x3f
getColor Tylo   = Just $ Rgb8Color 0xd4 0xab 0xab
getColor Gilly  = Just $ Rgb8Color 0x52 0x40 0x37
getColor Pol    = Just $ Rgb8Color 0x6f 0x72 0x56
getColor Dres   = Just $ Rgb8Color 0x2d 0x23 0x19
getColor Eeloo  = Just $ Rgb8Color 0x34 0x35 0x35


