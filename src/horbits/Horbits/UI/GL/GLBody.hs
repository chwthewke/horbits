module Horbits.UI.GL.GLBody where

import           Control.Lens
import           Control.Monad
import           Data.List
import           Data.Maybe
import           Data.Ord
import           Graphics.Rendering.OpenGL
import           Linear                                           hiding (trace)

import           Horbits.Body
import           Horbits.Data.StateVar
import           Horbits.DimLin                                   hiding (zero)
import           Horbits.Graphics.Rendering.OpenGL.PointSpriteARB
import           Horbits.KerbalDateTime
import           Horbits.Orbit
import           Horbits.UI.Camera
import           Horbits.UI.GL.GLIso

data DrawBodySpec = DrawBodySpec { drawZ        :: Double
                                 , drawPosition :: Vertex3 GLdouble
                                 , drawColor    :: Color4 GLfloat
--                                 , drawSize     :: GLfloat
                                 } deriving (Show)

-- TODO better drawbodyspecs with SolarSystem
drawBodySpec :: OrthoCamera Double -> KerbalInstant -> BodyId -> DrawBodySpec
drawBodySpec cam i b = DrawBodySpec z (pos ^. glVector) c
  where
    z = orthoCameraZIndex cam pos
    pos = fromMaybe zero $ b ^? bodyOrbit . to (`orbitPositionFromInstant` i) . to positionVector . from dim
    c = fromMaybe (Color4 1 1 0.7 1) $ b ^? bodyUiColor . to (set rgbaColorA 1) . glColorF

-- TODO really draw with different sizes
-- (tricky, sort by Z then group by size, or just do n renderPrimitive calls)
drawBodies :: TextureObject -> [DrawBodySpec] -> IO ()
drawBodies tex dbs =
    withStateVar (texture Texture2D) Enabled .
    withStateVar (textureBinding Texture2D) (Just tex) .
    withStateVar blend Enabled .
    withStateVar blendFunc (SrcAlpha, OneMinusSrcAlpha) .
    withStateVar depthFunc (Just Lequal) .
    withStateVar pointSpriteARB Enabled .
    withStateVar pointSize 16 $
        renderPrimitive Points $ forM_ (sortBy (comparing drawZ) dbs) $ \db -> do
            color $ drawColor db
            vertex $ drawPosition db


