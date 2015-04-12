{-# LANGUAGE TemplateHaskell #-}

module Horbits.Body.Atmosphere(
    Atmosphere(Atmosphere), atmosphereHeight, atmosphereScaleHeight, atmosphericPressure) where

import           Control.Lens

import           Horbits.Dimensional.Prelude

data Atmosphere = Atmosphere { _atmosphereHeight      :: Length Double
                             , _atmosphericPressure   :: Pressure Double
                             , _atmosphereScaleHeight :: Length Double
                             } deriving (Show, Eq)
makeLenses ''Atmosphere

