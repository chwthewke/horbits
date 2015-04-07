module Horbits.UI.Camera.Zoom (ZoomModel(..), linearZoom, geometricZoom, maxZoom) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.List.NonEmpty  as NE


newtype ZoomModel a = ZoomModel (NonEmpty a) deriving (Show, Eq)

maxZoom :: Getting a (ZoomModel a) a
maxZoom = to $ \(ZoomModel zooms) -> NE.last zooms

linearZoom :: (Num a, Ord a) => a -> (a, a) -> ZoomModel a
linearZoom step = ZoomModel . unfoldZooms (+ step)

geometricZoom :: (Num a, Ord a) => a -> (a, a) -> ZoomModel a
geometricZoom step = ZoomModel . unfoldZooms (* step)

unfoldZooms :: Ord a => (a -> a) -> (a, a) -> NonEmpty a
unfoldZooms nextScale (minScale, maxScale) = NE.unfold f minScale
  where
    f s = (s, nextScale <$> mfilter (< maxScale) (Just s))
