{-# LANGUAGE TemplateHaskell #-}


module Horbits.Orbit where

import           Control.Lens                         hiding ((*~))
import           Horbits.Quantities
import           Linear.V3                            (V3 (..))
import           Numeric.NumType                      (pos2, pos3)
import           Numeric.Units.Dimensional            (Dimensionless, Length,
                                                       Time, (*~), (/), (^))
import           Numeric.Units.Dimensional.Quantities (GravitationalParameter)
import           Numeric.Units.Dimensional.SIUnits
import           Prelude                              hiding ((*), (/), (^))

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
getBody Kerbin = mkBody 3.5316e12 600000
getBody _ = undefined

mkBody :: Double -> Double -> Body
mkBody mu r = Body (mu *~ (meter ^ pos3 / second ^ pos2)) (r *~ meter)

data Body = Body { _mu     :: GravitationalParameter Double
                 , _radius :: Length Double
                 }

data Orbit = Orbit { _body :: Body
                   , _h    :: ReducedAngularMomentum (V3 Double)
                   , _e    :: Dimensionless (V3 Double)
                   , _M0   :: Time Double
                   }

makeLenses ''Body
makeLenses ''Orbit
