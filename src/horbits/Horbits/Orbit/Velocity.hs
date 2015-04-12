{-# LANGUAGE TemplateHaskell #-}

module Horbits.Orbit.Velocity(OrbitalVelocity(OrbitalVelocity), orbitalVelocityMin, orbitalVelocityMax) where

import           Control.Lens

import           Horbits.Dimensional.Prelude

data OrbitalVelocity = OrbitalVelocity { _orbitalVelocityMin :: Velocity Double
                                       , _orbitalVelocityMax :: Velocity Double
                                       } deriving (Show, Eq)
makeLenses ''OrbitalVelocity
