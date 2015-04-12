module Horbits.OpenGL.PointSpriteARB(pointSpriteARB) where

import           Control.Applicative
import           Graphics.Rendering.OpenGL
import           Graphics.Rendering.OpenGL.Raw

pointSpriteARB :: StateVar Capability
pointSpriteARB = makeStateVar
    (unmarshalCapability <$> glIsEnabled gl_POINT_SPRITE_ARB)
    (\e -> if e == Enabled
            then do
                glEnable gl_POINT_SPRITE_ARB
                glTexEnvi gl_POINT_SPRITE_ARB gl_COORD_REPLACE_ARB (fromIntegral gl_TRUE)
            else glDisable gl_POINT_SPRITE_ARB)

unmarshalCapability :: GLboolean -> Capability
unmarshalCapability x = if  x /= fromIntegral gl_FALSE then Enabled else Disabled
