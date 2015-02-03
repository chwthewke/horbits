{-# LANGUAGE TemplateHaskell #-}

module Horbits.Body where

import           Control.Lens                         hiding ((*~))
import           Numeric.Units.Dimensional.TF.Prelude
import           Prelude                              hiding ((/), (^))


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

data Atmosphere = Atmosphere { _atmosphereHeight      :: Length Double
                             , _atmosphericPressure   :: Pressure Double
                             , _atmosphereScaleHeight :: Length Double
                             } deriving (Show, Eq)
makeLenses ''Atmosphere

data Body = Body { _bodyGravitationalParam :: GravitationalParameter Double
                 , _bodyRadius             :: Length Double
                 , _bodySphereOfInfluence  :: Maybe (Length Double)
                 , _bodyAtmosphere         :: Maybe Atmosphere
                 } deriving (Show, Eq)
makeLenses ''Body

bodyAtmosphereHeight :: Getter Body (Maybe (Length Double))
bodyAtmosphereHeight = pre (bodyAtmosphere . traverse . atmosphereHeight)

getBody :: BodyId -> Body
getBody Kerbol = mkBody 1.1723328e18 261600000 0 Nothing
getBody Moho = mkBody 1.6860938e11 250000 9.6466630e6 Nothing
getBody Eve = mkBody 8.1717302e12 700000 8.5109365e7 $ mkAtmosphere 96708.574 506.625 7000
getBody Gilly = mkBody 8.2894498e6 13000 1.2612327e5 Nothing
getBody Kerbin = mkBody 3.5316e12 600000 8.4159286e7 $ mkAtmosphere 69077.53 101.325 5000
getBody Mun = mkBody 6.5138398e10 200000 2.4295591e6 Nothing
getBody Minmus = mkBody 1.7658000e9 60000 2.2474284e6 Nothing
getBody Duna = mkBody 3.0136321e11 320000 4.7921949e7 $ mkAtmosphere 41446.532 20.2650 3000
getBody Ike = mkBody 1.8568369e10 130000 1.0495989e6 Nothing
getBody Dres = mkBody 2.1484489e10 138000 3.2832840e7 Nothing
getBody Jool = mkBody 2.8252800e14 6000000 2.4559852e9 $ mkAtmosphere 138155.11 1519.88 10000
getBody Laythe = mkBody 1.9620000e12 500000 3.7236458e6 $ mkAtmosphere 55262.042 81.0600 4000
getBody Vall = mkBody 2.0748150e11 300000 2.4064014e6 Nothing
getBody Tylo = mkBody 2.8252800e12 600000 1.0856518e7 Nothing
getBody Bop = mkBody 2.4868349e9 65000 1.2210609e6 Nothing
getBody Pol = mkBody 7.2170208e8 44000 1.0421389e6 Nothing
getBody Eeloo = mkBody 7.4410815e10 210000 1.1908294e8 Nothing

mkAtmosphere :: Double -> Double -> Double -> Maybe Atmosphere
mkAtmosphere h p s = Just $ Atmosphere (h *~ meter) (p *~ kilo pascal) (s *~ meter)

mkBody :: Double -> Double -> Double -> Maybe Atmosphere -> Body
mkBody mu r soi = Body (mu *~ (meter ^ pos3 / second ^ pos2))
                       (r *~ meter)
                       (if soi == 0 then Nothing else Just (soi *~ meter))

fromBodyId :: Getter BodyId Body
fromBodyId = to getBody

