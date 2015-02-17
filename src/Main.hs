module Main where

import           Control.Lens                  hiding (set)
import           Foreign.Marshal.Array
import           Graphics.Rendering.OpenGL.GL  as GL
import           Graphics.Rendering.OpenGL.GLU
import           Graphics.UI.Gtk
import           Graphics.UI.Gtk.OpenGL
import           Horbits.Body
import           Horbits.SolarSystem
import           Horbits.UI.BodyDetails
import           Horbits.UI.BodyList

main :: IO ()
main = do
    _ <- initGUI
    window <- windowNew
    _ <- set window [ windowTitle := "Hello" ]
    box <- hBoxNew True 5
    containerAdd window box
    -- Actual layout
    containerAdd box =<< bodyDataPane
    containerAdd box =<< initGlCanvas 600 400 drawCanvas
    --
    widgetShowAll window
    _ <- onDestroy window mainQuit
    mainGUI


bodyDataPane :: IO VBox
bodyDataPane = do
    box <- vBoxNew True 5
    bodyList <- bodyListNew bodiesTree
    containerAdd box $ bodyListView bodyList
    bodyDetailsPane <- bodyDetailsPaneNew
    _ <- bodyListOnSelectionChange bodyList (\b -> do
        putStrLn $ "Selected " ++ b ^. bodyName
        bodyDetailsPaneSetBody bodyDetailsPane b)
    containerAdd box $ bodyDetailsPaneView bodyDetailsPane
    return box



initGlCanvas :: Int -> Int -> IO() -> IO GLDrawingArea
initGlCanvas w h draw = do
    glConfig <- glConfigNew [GLModeRGBA, GLModeDepth, GLModeDouble]
    canvas <- glDrawingAreaNew glConfig
    widgetSetSizeRequest canvas w h
    _ <- onRealize canvas $ withGLDrawingArea canvas setupProj
    setupDrawLoop canvas draw
    return canvas

setupProj :: GLWindow -> IO ()
setupProj _ = do
  clearColor $= Color4 0.0 0.0 1.0 0.0
  matrixMode $= Projection
  loadIdentity
  ortho (-1.0) 1.0 (-1.0) 1.0 (-1.0) 1.0
  depthFunc $= Just Less
  drawBuffer $= BackBuffers

setupDrawLoop :: GLDrawingArea -> IO () -> IO ()
setupDrawLoop canvas draw = do
    _ <- onExpose canvas . const $ withGLDrawingArea canvas drawLoop >> return True
    _ <- timeoutAddFull (widgetQueueDraw canvas >> return True) priorityDefaultIdle 8
    return ()
  where
    drawLoop glWin = do
        clear [DepthBuffer, ColorBuffer]
        draw
        glDrawableSwapBuffers glWin

drawCanvas :: IO ()
drawCanvas = do
    drawEllipse (Color3 1.0 0.5 0.0 :: Color3 GLfloat) 0.7 0.3
    drawEllipse (Color3 0.0 1.0 0.5 :: Color3 GLfloat) 0.6 0.2
    color (Color3 1.0 1.0 1.0 :: Color3 GLfloat)
    renderPrimitive Polygon $ do
        vertex (Vertex3 0.0 0.0 0.0 :: Vertex3 GLfloat)
        vertex (Vertex3 1.0 0.0 0.0 :: Vertex3 GLfloat)
        vertex (Vertex3 0.0 1.0 0.0 :: Vertex3 GLfloat)
    GL.flush

drawEllipse :: (GL.Color c) =>  c -> GLfloat -> GLfloat -> IO ()
drawEllipse c a b = do
    color c
    withNURBSObj () $ \nurbsObj ->
        nurbsBeginEndCurve nurbsObj $
            withArrayLen knots $ \nKnots knots' ->
                withArray controls $ \controls' ->
                    nurbsCurve nurbsObj (fromIntegral nKnots) knots' 4 controls' 3
  where
    knots = [0, 0, 0, 0.25, 0.25, 0.5, 0.5, 0.75, 0.75, 1, 1, 1]
    weights = let q = sqrt 0.5 in [1, q, 1, q, 1, q, 1, q, 1]
    points = [(a, 0), (a, b), (0, b),
              (-a, b), (-a, 0), (-a, -b),
              (0, -b), (a, -b), (a, 0)]
    mkControl (x, y) w = Vertex4 (x*w) (y*w) 0 w :: Vertex4 GLfloat
    controls = zipWith mkControl points weights





