{-# LANGUAGE TemplateHaskell #-}

module Horbits.UI.Model where

import           Control.Lens
import           Data.Binding.Simple

import           Horbits.Body
import           Horbits.KerbalDateTime
import           Horbits.UI.Camera

data UIModel = UIModel { _modelCamera       :: OrthoCamera Double
                       , _modelSelectedBody :: Maybe Body
                       , _modelTime         :: KerbalInstant
                       }

makeLenses ''UIModel

uiModelNew :: Variable v => IO (Source v UIModel)
uiModelNew = newVar $ UIModel (initOrthoCamera (geometricZoom 1.2 (1e6, 1e12)))
                              Nothing
                              epoch
