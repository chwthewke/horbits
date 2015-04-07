module HorbitsCC.CameraControlExploration(basesAndCameraControls, ZoomModel(..), linearZoom, geometricZoom) where

import           Control.Lens
import           Control.Monad             hiding (forM_)
import           Data.Binding.Simple
import           Data.Foldable
import           Data.IORef
import           Graphics.Rendering.OpenGL
import           Graphics.UI.Gtk           hiding (set)
import           Horbits.UI.Camera
import           Horbits.UI.Camera.Control
import           Horbits.UI.Camera.Zoom
import           Horbits.UI.GL.GLCamera
import           Horbits.UI.GL.GLSetup
import           Linear
import           Prelude                   hiding (foldr)

-- Cam tests

drawBasis :: (VertexComponent a, RealFloat a) => Vertex4 a -> IO ()
drawBasis (Vertex4 x y z w) =
    renderPrimitive Lines $ do
        color (Color3 1.0 0.0 0.0 :: Color3 GLfloat)
        vertex $ Vertex3 x y z
        vertex $ Vertex3 (x + w) y z
        color (Color3 0.0 1.0 0.0 :: Color3 GLfloat)
        vertex $ Vertex3 x y z
        vertex $ Vertex3 x (y + w) z
        color (Color3 0.0 0.0 1.0 :: Color3 GLfloat)
        vertex $ Vertex3 x y z
        vertex $ Vertex3 x y (z + w)

drawBases :: IO ()
drawBases = do
    drawBasis (Vertex4 0.0 0.0 0.0 1.0 :: Vertex4 GLfloat)
    drawBasis (Vertex4 2.0 0.0 0.0 0.7 :: Vertex4 GLfloat)
    drawBasis (Vertex4 (-2.0) 0.0 0.0 0.7 :: Vertex4 GLfloat)
    drawBasis (Vertex4 0.0 2.0 0.0 0.7 :: Vertex4 GLfloat)
    drawBasis (Vertex4 0.0 (-2.0) 0.0 0.7 :: Vertex4 GLfloat)
    drawBasis (Vertex4 0.0 0.0 2.0 0.7 :: Vertex4 GLfloat)
    drawBasis (Vertex4 0.0 0.0 (-2.0) 0.7 :: Vertex4 GLfloat)

degrees :: (Floating a) => Iso' a a
degrees = iso (* (180 / pi)) (* (pi / 180))

orthoCameraColatitudeDeg :: Floating a => Lens' (OrthoCamera a) a
orthoCameraColatitudeDeg = orthoCameraColatitude . degrees

orthoCameraLongitudeDeg :: Floating a => Lens' (OrthoCamera a) a
orthoCameraLongitudeDeg = orthoCameraLongitude . degrees

basesAndCameraControls :: Window -> IO ()
basesAndCameraControls window = do
    cam <- newVar $ orthoCamera (linearZoom 1 (1, 20)) 600 600 :: IO (Source IORef (OrthoCamera Double))
    box <- hBoxNew False 5
    ctrlBox <- vBoxNew True 5
    containerAdd box ctrlBox
    boundSpinButton cam (orthoCameraCenter . _x) "x" (-5) 5 1 (Just 0) >>= containerAdd ctrlBox
    boundSpinButton cam (orthoCameraCenter . _y) "y" (-5) 5 1 (Just 0) >>= containerAdd ctrlBox
    boundSpinButton cam (orthoCameraCenter . _z) "z" (-5) 5 1 (Just 0) >>= containerAdd ctrlBox
    boundSpinButton cam orthoCameraScale "d" 1 20 1 Nothing >>= containerAdd ctrlBox
    boundSpinButton cam orthoCameraColatitudeDeg "colat" 0 180 5 Nothing >>= containerAdd ctrlBox
    boundSpinButton cam orthoCameraLongitudeDeg "long" 0 360 5 Nothing >>= containerAdd ctrlBox
    resizeCb <- bindCameraToGL cam
    canvas <- setupGL 600 600 resizeCb drawBases
    _ <- setupMouseControl canvas cam
    containerAdd box canvas
    containerAdd window box

boundSpinButton :: (Variable v) => v t -> ASetter' t Double -> String -> Double -> Double -> Double -> Maybe Double -> IO HBox
boundSpinButton src prop lab min' max' step def = do
    box <- hBoxNew True 5
    sb <- spinButtonNewWithRange min' max' step
    forM_ def $ spinButtonSetValue sb
    _ <- onValueSpinned sb $ spinButtonGetValue sb >>= (modifyVar src . set prop)
    _ <- labelNew (Just lab) >>= containerAdd box
    containerAdd box sb
    return box

