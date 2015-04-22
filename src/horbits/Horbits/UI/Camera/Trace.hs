{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes      #-}


module Horbits.UI.Camera.Trace(logCamera, orthoCameraColatitudeDeg, orthoCameraLongitudeDeg) where

import           Control.Lens
import           Data.Foldable              (mapM_)
import           Data.StateVar
import           Linear
import           Prelude                    hiding (mapM_)
import           Text.Printf.TH

import           Horbits.Data.Binding
import           Horbits.UI.Camera.Internal

degrees :: (Floating a) => Iso' a a
degrees = iso (* (180 / pi)) (* (pi / 180))

orthoCameraColatitudeDeg :: Floating a => Lens' (OrthoCamera a) a
orthoCameraColatitudeDeg = orthoCameraColatitude . degrees

orthoCameraLongitudeDeg :: Floating a => Lens' (OrthoCamera a) a
orthoCameraLongitudeDeg = orthoCameraLongitude . degrees


logCamera :: (HasGetter v (OrthoCamera a), Show a, RealFloat a, Epsilon a) => v -> IO ()
logCamera cam = do
    c <- readVar cam
    putStrLn $ [s|O: %.2e %.2e %.2e|] (c ^. orthoCameraCenter . _x)
                                      (c ^. orthoCameraCenter . _y)
                                      (c ^. orthoCameraCenter . _z)
    putStrLn $ [s|r: %.2e th: %.2e la: %.2e|] (c ^. orthoCameraScale)
                                              (c ^. orthoCameraLongitudeDeg)
                                              (c ^. orthoCameraColatitudeDeg)
    mapM_ showMRow . orthoCameraMatrix $ c
  where
    showMRow (V4 x y z w) = putStrLn $ [s|%8.2e %8.2e %8.2e %8.2e|] x y z w

