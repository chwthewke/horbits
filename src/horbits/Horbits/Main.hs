

module Horbits.Main(main) where

import           Control.Applicative
import           Control.Lens                 hiding (set)
import           Data.Foldable                (forM_)
import           Data.Tree
import           Graphics.Rendering.OpenGL.GL as GL
import           Graphics.UI.Gtk
import           Horbits.Body
import           Horbits.Orbit
import           Horbits.Orbit.Geometry
import           Horbits.SolarSystem
import           Horbits.UI.BodyDetails
import           Horbits.UI.BodyList
import           Horbits.UI.GL.GLCamera
import           Horbits.UI.GL.GLOrbit
import           Horbits.UI.GL.GLSetup
import qualified Linear                       as L
import           Numeric.Units.Dimensional.TF (Dimensional (Dimensional))

main :: IO ()
main = do
    _ <- initGUI
    window <- windowNew
    layoutDisplay window
    --
    widgetShowAll window
    _ <- onDestroy window mainQuit
    mainGUI

layoutDisplay :: Window -> IO ()
layoutDisplay window = do
    _ <- set window [ windowTitle := "Hello" ]
    -- Actual layout
    box <- hBoxNew True 5
    containerAdd window box
    containerAdd box =<< bodyDataPane
    rBox <- vBoxNew False 5
    containerAdd box rBox
    containerAdd rBox =<< setupGL 600 600 (\w h -> projMatrix 5e11 w h >>= setGLCamera) drawCanvas


bodyDataPane :: IO VBox
bodyDataPane = do
    box <- vBoxNew False 5
    bodyList <- bodyListNew [bodiesTree]
    containerAdd box $ bodyListView bodyList
    bodyDetailsPane <- bodyDetailsPaneNew
    _ <- bodyListOnSelectionChange bodyList (\b -> do
        putStrLn $ "Selected " ++ b ^. bodyName
        bodyDetailsPaneSetBody bodyDetailsPane b)
    containerAdd box $ bodyDetailsPaneView bodyDetailsPane
    return box


drawPlanetOrbit :: RgbaFColor -> Orbit -> IO ()
drawPlanetOrbit col orbit = do
    let ce = orbit ^. centralOrbit
    let Dimensional c = ce ^. center
    let Dimensional a = ce ^. semiMajorAxisVector
    let Dimensional b = ce ^. semiMinorAxisVector
    drawEllipse3d col c a b

planets :: [BodyId]
planets = map (view bodyId . rootLabel) . subForest $ bodiesTree

planetOrbits :: [(RgbaFColor, Orbit)]
planetOrbits = planets >>= getOrbit
  where
    getOrbit bId = (,) <$> (bId ^.. bodyUiColor) <*> (bId ^.. bodyOrbit)


projMatrix :: Integral a => Double -> a -> a -> IO (L.M44 Double)
projMatrix d w h = return $ L.ortho (-r) r (-b) b (-d) d
  where
    (r, b) = if w > h
                then (d * fromIntegral w / fromIntegral h, d)
                else (d, d * fromIntegral h / fromIntegral w)

drawCanvas :: IO ()
drawCanvas = do
    forM_ planetOrbits (uncurry drawPlanetOrbit)
    GL.flush

