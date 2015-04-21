{-# LANGUAGE TemplateHaskell #-}

module Horbits.UI.Model where

import           Control.Lens
import           Control.Monad               (void)
import           System.Clock

import           Horbits.Body
import           Horbits.Data.Binding
import           Horbits.Dimensional.Prelude (dim)
import           Horbits.Orbit
import           Horbits.Time
import           Horbits.UI.Camera

data UIModel = UIModel { _modelCamera       :: OrthoCamera Double
                       , _modelSelectedBody :: Maybe Body
                       , _modelClock        :: KerbalClock
                       , _modelTick         :: TimeSpec
                       } deriving (Show, Eq)

makeLenses ''UIModel

modelTime :: Getter UIModel KerbalInstant
modelTime = to $ do
    clock <- view modelClock
    tick <- view modelTick
    return $ readClock clock tick

uiModelNew :: IO (IORefBindingSource UIModel)
uiModelNew = do
    now <- getTime Monotonic
    model <- newVar $ UIModel (initOrthoCamera (geometricZoom 1.2 (1e6, 1e12))) Nothing (StoppedClock epoch) now
    void $ bindEq (mapVarL modelTick model) $ const $ model $~ updateCamera
    void $ bindEq (mapVarL modelClock model) $ const $ model $~ updateCamera
    void $ bindEq (mapVarL modelSelectedBody model) $ const $ model $~ updateCamera
    return model
  where
    updateCamera = do
        instant <- view modelTime
        selectedBody <- view modelSelectedBody
        modelCamera %~ maybe id (lookAtBody instant) selectedBody
    lookAtBody instant body = orthoCameraCenter .~ (bodyPosition instant (body ^. bodyId) ^. from dim)

