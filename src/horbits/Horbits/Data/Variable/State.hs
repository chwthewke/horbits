module Horbits.Data.Variable.State (runStateVar) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Data.Variable

runStateVar :: (MonadIO m, Variable v) => v s -> StateT s m a -> m a
runStateVar v st = do
    i <- liftIO $ readVar v
    (r, o) <- runStateT st i
    _ <- liftIO $ writeVar v o
    return r

