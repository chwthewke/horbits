module Horbits.Data.StateVar where

import Control.Monad.IO.Class
import           Control.Monad.Trans.State (StateT, runStateT)
import Data.StateVar

withStateVar :: (HasGetter s a, HasSetter s a, MonadIO m) => s -> a -> m b -> m b
withStateVar st a b = do
    old <- liftIO $ get st
    liftIO $ st $= a
    r <- b
    liftIO $ st $= old
    return r

evalStateVar :: (HasGetter s a, HasSetter s a, MonadIO m) => s -> StateT a m b -> m b
evalStateVar v st = do
    i <- liftIO $ get v
    (r, o) <- runStateT st i
    liftIO $ v $= o
    return r
