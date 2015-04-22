module Horbits.UI.GL.GLOrbit(drawOrbits) where

import           Control.Applicative
import           Control.Lens
import           Data.Foldable                 (forM_)
import           Foreign.Marshal.Array
import           Graphics.Rendering.OpenGL.GL  as GL
import           Graphics.Rendering.OpenGL.GLU
import           Linear

import           Horbits.Body
import           Horbits.Data.StateVar
import           Horbits.Dimensional.Prelude   (dim)
import           Horbits.Orbit
import           Horbits.Time
import           Horbits.UI.GL.GLIso

drawOrbits :: KerbalInstant -> [BodyId] -> IO ()
drawOrbits = (withStateVar lineSmooth Enabled .) . mapM_ . drawOrbit

drawOrbit :: KerbalInstant -> BodyId -> IO ()
drawOrbit time body = forM_ (colorAndOrbit body) $ \(col, orbit) -> do
    let o = orbit ^. orbitCenter . from dim
    let f = bodyPosition time $ orbit ^. orbitBodyId
    let c = o ^+^ f ^. from dim
    let a = orbit ^. orbitSemiMajorAxisVector . from dim
    let b = orbit ^. orbitSemiMinorAxisVector . from dim
    drawEllipse3d col c a b

colorAndOrbit :: BodyId -> Maybe (RgbaColor Float, Orbit)
colorAndOrbit b = (,) <$> b ^? bodyUiColor <*> b ^? bodyOrbit

-- | drawEllipse3d color center semiMajor semiMinor
drawEllipse3d :: RgbaColor Float -> V3 Double -> V3 Double -> V3 Double -> IO ()
drawEllipse3d c o a b = do
    color $ (c & rgbaColorA .~ 1) ^. glColorF
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
        (realToFrac <$> w *^ (V4 0 0 0 1 & _xyz .~ o ^+^ px *^ a + py *^ b) :: V4 Float) ^. glV4
    controls = zipWith3 mkControl weights pxs pys

