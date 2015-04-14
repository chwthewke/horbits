{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE UndecidableInstances      #-}


module Horbits.Data.Binding where

import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Function
import           Data.IORef
import           Data.StateVar
import           GHC.Conc


class (HasGetter v a, HasUpdate v a a) => Variable v a | v -> a where
    newVar :: MonadIO m => a -> m v

instance Variable (IORef a) a where
    newVar = liftIO . newIORef

instance Variable (TVar a) a where
    newVar = liftIO . newTVarIO

class HasGetter v a => Bindable v a | v -> a where
    bind :: MonadIO m
         => v
         -> (a -> a -> IO ())
         -> m ()


newtype WrappedGetter a b = WrappedGetter (Getter a b)
newtype WrappedSetter a b = WrappedSetter (Setter' a b)
newtype WrappedLens a b = WrappedLens (Lens' a b)

data MappedVariable v o a b = MappedVariable v (o a b)

instance HasGetter v a => HasGetter (MappedVariable v WrappedGetter a b) b where
    get (MappedVariable a (WrappedGetter g)) = getThrough g a

instance HasGetter v a => HasGetter (MappedVariable v WrappedLens a b) b where
    get (MappedVariable a (WrappedLens g)) = getThrough g a

getThrough :: (HasGetter v a, MonadIO m) => Getter a b -> v -> m b
getThrough g = liftM (view g) . get

instance HasUpdate v a a => HasSetter (MappedVariable v WrappedSetter a b) b where
    ($=) (MappedVariable a (WrappedSetter s)) = setThrough s a

instance HasUpdate v a a => HasSetter (MappedVariable v WrappedLens a b) b where
    ($=) (MappedVariable a (WrappedLens s)) = setThrough s a

setThrough :: (HasUpdate v a a, MonadIO m) => Setter' a b -> v -> b -> m ()
setThrough s a b = a $~ (s .~ b)

instance HasUpdate v a a => HasUpdate (MappedVariable v WrappedSetter a b) b b where
    ($~) (MappedVariable a (WrappedSetter s)) = ($~) `through` s $ a
    ($~!) (MappedVariable a (WrappedSetter s)) = ($~!) `through` s $ a

instance HasUpdate v a a => HasUpdate (MappedVariable v WrappedLens a b) b b where
    ($~) (MappedVariable a (WrappedLens s)) = ($~) `through` s $ a
    ($~!) (MappedVariable a (WrappedLens s)) = ($~!) `through` s $ a

through :: (v -> (a -> a) -> m ()) -> Setter' a b -> v -> (b -> b) -> m ()
through o s v f = v `o` (s %~ f)

instance Bindable v a => Bindable (MappedVariable v WrappedLens a b) b where
    bind (MappedVariable a (WrappedLens g)) = bindThrough g a

bindThrough :: (Bindable v a, MonadIO m) => Getter a b -> v -> (b -> b -> IO ()) -> m ()
bindThrough g v f = bind v (f `on` view g)

bindConst :: (Bindable v a, MonadIO m) => v -> (a -> IO ()) -> m ()
bindConst v = bind v . const

bindEq :: (Eq a, Bindable v a, MonadIO m) => v -> (a -> IO ()) -> m ()
bindEq v f = bind v $ \old new -> unless (old == new) (f new)

-- Source

data Binding a = Binding (a -> a -> IO ())

data BindingSource v a b = (HasGetter v a, HasUpdate v a a, Variable b [Binding a])
                         => BindingSource v b

mkSource :: (HasGetter v a, HasUpdate v a a, Variable b [Binding a]) => v -> IO (BindingSource v a b)
mkSource v = do
    bindings <- newVar []
    return $ BindingSource v bindings

instance HasGetter (BindingSource v a b) a where
    get (BindingSource v _) = get v

instance HasSetter (BindingSource v a b) a where
    ($=) s@(BindingSource v _) a = do
        v $= a
        liftIO $ update s

instance HasUpdate (BindingSource v a b) a a

update :: BindingSource v a b -> IO ()
update (BindingSource v b) = do
    bs <- get b
    a <- get v
    forM_ bs $ \(Binding f) -> f a a

instance Bindable (BindingSource v a b) a where
  bind (BindingSource v b) f = do
    b $~ (Binding f :)
    a <- get v
    liftIO $ f a a


