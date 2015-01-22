{-# LANGUAGE TemplateHaskell #-}

module Horbits.Body where

import           Control.Applicative
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

getBody :: BodyId -> Body
getBody Kerbin = mkBody 3.5316e12 600000 (Just 70000)
getBody _ = undefined

fromBodyId :: Getter BodyId Body
fromBodyId = to getBody

mkBody :: Double -> Double -> Maybe Double -> Body
mkBody mu r atm = Body (mu *~ (meter ^ pos3 / second ^ pos2)) (r *~ meter) ((*~ meter) <$> atm)

data Body = Body { _gravitationalParam   :: GravitationalParameter Double
                 , _bodyRadius           :: Length Double
                 , _bodyAtmosphereHeight :: Maybe (Length Double)
                 } deriving (Show, Eq)
makeLenses ''Body
