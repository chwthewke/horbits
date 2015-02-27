module Horbits.UI.GL.GLOrbit(drawEllipse3d, drawEllipse, drawEllipse') where

import           Control.Lens
import           Foreign.Marshal.Array
import           Graphics.Rendering.OpenGL.GL  as GL
import           Graphics.Rendering.OpenGL.GLU
import           Horbits.Body
import           Horbits.UI.GL.GLIso
import           Linear.V3
import           Linear.Vector

drawEllipse' :: Rgb8Color -> GLfloat -> GLfloat -> IO ()
drawEllipse' c = drawEllipse (c ^. glColor)

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

-- | drawOrbit color center semiMajor semiMinor
drawEllipse3d :: Rgb8Color -> V3 Double -> V3 Double -> V3 Double -> IO ()
drawEllipse3d c o a b = do
    color $ c ^. glColor
    withNURBSObj () $ \nurbsObj -> do
        setSamplingMethod nurbsObj $ PathLength 4
        nurbsBeginEndCurve nurbsObj $
            withArrayLen knots $ \nK ks ->
                withArray controls $ \cs ->
                    nurbsCurve nurbsObj (fromIntegral nK) ks 4 cs 3
  where
    knots = [0, 0, 0, 0.25, 0.25, 0.5, 0.5, 0.75, 0.75, 1, 1, 1]
    weights = take 9 $ cycle [1, sqrt 0.5]
    pxs = [1, 1, 0, -1, -1, -1, 0, 1, 1]
    pys = [0, 1, 1, 1, 0, -1, -1, -1, 0]
    mkControl w px py =
        let (V3 x y z) = o ^+^ px *^ a + py *^ b
        in Vertex4 (realToFrac x * w) (realToFrac y * w) (realToFrac z * w) w :: Vertex4 GLfloat
    controls = zipWith3 mkControl weights pxs pys

