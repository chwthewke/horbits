module Horbits.Data.StateVar where

import Control.Monad.IO.Class
import Graphics.Rendering.OpenGL

withStateVar :: (HasGetter s a, HasSetter s a, MonadIO m) => s -> a -> m b -> m b
withStateVar st a b = do
    old <- liftIO $ get st
    liftIO $ st $= a
    r <- b
    liftIO $ st $= old
    return r
