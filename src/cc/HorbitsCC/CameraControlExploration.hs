{-# LANGUAGE QuasiQuotes #-}

module HorbitsCC.CameraControlExploration(basesAndCameraControls, ZoomModel(..), linearZoom, geometricZoom) where

import           Control.Lens
import           Control.Monad             hiding (forM_, mapM_)
import           Control.Monad.IO.Class
import           Data.Binding.Simple
import           Data.Foldable
import           Graphics.Rendering.OpenGL
import           Graphics.UI.Gtk           hiding (set)
import           Linear
import           Prelude                   hiding (foldr, mapM_)
import           Text.Printf.TH

import           Horbits.UI.Camera
import           Horbits.UI.GL.GLSetup

-- Cam tests

drawSimplex :: Vertex4 GLdouble -> IO ()
drawSimplex (Vertex4 x y z w) =
    let (o, a, b, c) = ( Vertex3 x y z
                       , Vertex3 (x+w) y z
                       , Vertex3 x (y+w) z
                       , Vertex3 x y (z+w)
                       ) in
        renderPrimitive Triangles $ do
            color (Color3 1 0 0 :: Color3 GLfloat)
            vertex o
            vertex a
            vertex b
            color (Color3 0 1 0 :: Color3 GLfloat)
            vertex o
            vertex a
            vertex c
            color (Color3 0 0 1 :: Color3 GLfloat)
            vertex o
            vertex b
            vertex c
            color (Color3 1 1 1 :: Color3 GLfloat)
            vertex a
            vertex b
            vertex c

drawBases :: IO ()
drawBases = do
    drawSimplex (Vertex4 0.0 0.0 0.0 1.0)
    drawSimplex (Vertex4 2.0 0.0 0.0 0.7)
    drawSimplex (Vertex4 (-2.0) 0.0 0.0 0.7)
    drawSimplex (Vertex4 0.0 2.0 0.0 0.7)
    drawSimplex (Vertex4 0.0 (-2.0) 0.0 0.7)
    drawSimplex (Vertex4 0.0 0.0 2.0 0.7)
    drawSimplex (Vertex4 0.0 0.0 (-2.0) 0.7)

degrees :: (Floating a) => Iso' a a
degrees = iso (* (180 / pi)) (* (pi / 180))

orthoCameraColatitudeDeg :: Floating a => Lens' (OrthoCamera a) a
orthoCameraColatitudeDeg = orthoCameraColatitude . degrees

orthoCameraLongitudeDeg :: Floating a => Lens' (OrthoCamera a) a
orthoCameraLongitudeDeg = orthoCameraLongitude . degrees

basesAndCameraControls :: Window -> IO ()
basesAndCameraControls window = do
    (canvas, cam) <- setupGLWithCamera 600 600 $ initOrthoCamera (linearZoom 1 (1, 20))
    onGtkGLInit canvas $ onGtkGLDraw canvas drawBases
    box <- hBoxNew False 5
    ctrlBox <- vBoxNew True 5
    containerAdd box ctrlBox
    boundSpinButton cam (orthoCameraCenter . _x) "x" (-5) 5 1 (Just 0) >>= containerAdd ctrlBox
    boundSpinButton cam (orthoCameraCenter . _y) "y" (-5) 5 1 (Just 0) >>= containerAdd ctrlBox
    boundSpinButton cam (orthoCameraCenter . _z) "z" (-5) 5 1 (Just 0) >>= containerAdd ctrlBox
    boundSpinButton cam orthoCameraScale "d" 1 20 1 Nothing >>= containerAdd ctrlBox
    boundSpinButton cam orthoCameraColatitudeDeg "colat" 0 180 5 Nothing >>= containerAdd ctrlBox
    boundSpinButton cam orthoCameraLongitudeDeg "long" 0 360 5 Nothing >>= containerAdd ctrlBox
    containerAdd box canvas
    containerAdd window box
    void $ on window keyPressEvent $ tryEvent $ do
        [Control] <- eventModifier
        "k" <- eventKeyName
        liftIO $ logCamera cam

boundSpinButton :: (Variable v) =>
                    v t -> ASetter' t Double -> String -> Double -> Double -> Double -> Maybe Double -> IO HBox
boundSpinButton src prop lab min' max' step def = do
    box <- hBoxNew True 5
    spb <- spinButtonNewWithRange min' max' step
    forM_ def $ spinButtonSetValue spb
    _ <- onValueSpinned spb $ spinButtonGetValue spb >>= (modifyVar src . set prop)
    _ <- labelNew (Just lab) >>= containerAdd box
    containerAdd box spb
    return box

logCamera :: (Variable v, Show a, RealFloat a, Epsilon a) => v (OrthoCamera a) -> IO ()
logCamera cam = do
    c <- readVar cam
    putStrLn $ [s|O: %.2f %.2f %.2f|] (c ^. orthoCameraCenter . _x)
                                      (c ^. orthoCameraCenter . _y)
                                      (c ^. orthoCameraCenter . _z)
    putStrLn $ [s|r: %.2f th: %.2f la: %.2f|] (c ^. orthoCameraScale)
                                              (c ^. orthoCameraLongitudeDeg)
                                              (c ^. orthoCameraColatitudeDeg)
    mapM_ showMRow . orthoCameraMatrix $ c
  where
    showMRow (V4 x y z w) = putStrLn $ [s|%.2f %.2f %.2f %.2f|] x y z w

