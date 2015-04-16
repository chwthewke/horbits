{-# LANGUAGE TemplateHaskell #-}

module Horbits.UI.Model where

import           Control.Lens

import           Horbits.Body
import           Horbits.Data.Binding
import           Horbits.Time
import           Horbits.UI.Camera

data UIModel = UIModel { _modelCamera       :: OrthoCamera Double
                       , _modelSelectedBody :: Maybe Body
                       , _modelTime         :: KerbalInstant
                       } deriving (Show, Eq)

makeLenses ''UIModel

-- TODO type synonym in Data.Binding?

uiModelNew :: IO (IORefBindingSource UIModel)
uiModelNew = newVar $ UIModel (initOrthoCamera (geometricZoom 1.2 (1e6, 1e12))) Nothing epoch

