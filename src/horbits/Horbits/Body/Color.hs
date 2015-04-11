{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE TemplateHaskell #-}

module Horbits.Body.Color(RgbaColor(RgbaColor), rgbaColorR, rgbaColorG, rgbaColorB, rgbaColorA) where

import           Control.Lens

data RgbaColor a = RgbaColor { _rgbaColorR :: a
                             , _rgbaColorG :: a
                             , _rgbaColorB :: a
                             , _rgbaColorA :: a
                             } deriving (Show, Eq, Functor)

makeLenses ''RgbaColor
