{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE UndecidableInstances      #-}


module Horbits.Data.Binding(
    Variable(..), Bindable(..), BindingId(), HasBinding,
    BindingSource(), IORefBindingSource, TVarBindingSource,
    MappedVariable,
    readVar,
    mapVar, mapVarG, mapVarS, mapVarL, mapVarP,
    bindEq, bindConst,
    module Horbits.Data.StateVar,
    module Data.StateVar)
  where

import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.IORef
import           Data.Map               (Map)
import qualified Data.Map               as M
import           Data.Maybe             (fromMaybe)
import           Data.StateVar
import           GHC.Conc

import           Horbits.Data.StateVar

class (HasGetter v a, HasUpdate v a a) => Variable v a | v -> a where
    newVar :: MonadIO m => a -> m v

instance Variable (IORef a) a where
    newVar = liftIO . newIORef

instance Variable (TVar a) a where
    newVar = liftIO . newTVarIO

class Bindable v a | v -> a where
    bind :: MonadIO m
         => v
         -> (Maybe a -> a -> IO ())
         -> m BindingId
    unbind :: MonadIO m
           => v
           -> BindingId
           -> m ()

readVar :: (HasGetter v a, MonadIO m) => v -> m a
readVar = get

newtype WrappedGetter a b = WrappedGetter (Getter a b)
newtype WrappedSetter a b = WrappedSetter (Setter' a b)
newtype WrappedLens a b = WrappedLens (Lens' a b)

data MappedVariable v o a b = MappedVariable v (o a b)

mapVar :: (a -> b) -> v -> MappedVariable v WrappedGetter a b
mapVar f = mapVarG (to f)

mapVarG :: Getter a b -> v -> MappedVariable v WrappedGetter a b
mapVarG g v = MappedVariable v $ WrappedGetter g

mapVarS :: Setter' a b -> v -> MappedVariable v WrappedSetter a b
mapVarS g v = MappedVariable v $ WrappedSetter g

mapVarL :: Lens' a b -> v -> MappedVariable v WrappedLens a b
mapVarL l v = MappedVariable v $ WrappedLens l

mapVarP :: Prism' a b -> b -> v -> MappedVariable v WrappedLens a b
mapVarP p b v = MappedVariable v $ WrappedLens (prismToUnlawfulLens p b)

prismToUnlawfulLens :: Prism' a b -> b -> Lens' a b
prismToUnlawfulLens p b f a = fmap (p #) (f . fromMaybe b $ a ^? p)

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

instance Bindable v a => Bindable (MappedVariable v WrappedGetter a b) b where
    bind (MappedVariable a (WrappedGetter g)) = bindThrough g a
    unbind (MappedVariable a _) = unbind a

instance Bindable v a => Bindable (MappedVariable v WrappedLens a b) b where
    bind (MappedVariable a (WrappedLens g)) = bindThrough g a
    unbind (MappedVariable a _) = unbind a

bindThrough :: (Bindable v a, MonadIO m) => Getter a b -> v -> (Maybe b -> b -> IO ()) -> m BindingId
bindThrough g v f = bind v $ \old new -> f (fmap (view g) old) (view g new)

bindConst :: (Bindable v a, MonadIO m) => v -> (a -> IO ()) -> m BindingId
bindConst v = bind v . const

bindEq :: (Eq a, Bindable v a, MonadIO m) => v -> (a -> IO ()) -> m BindingId
bindEq v f = bind v $ \old new -> unless (old == Just new) (f new)

-- Source

data Binding a = Binding (Maybe a -> a -> IO ())

data BindingId = BindingId Int

data BindingSource v a b = (HasGetter v a, HasUpdate v a a, Variable b (Map Int (Binding a)))
                         => BindingSource v b

type IORefBindingSource a = BindingSource (IORef a) a (IORef (Map Int (Binding a)))
type TVarBindingSource a = BindingSource (TVar a) a (TVar (Map Int (Binding a)))

class (HasUpdate v a a, Bindable v a) => HasBinding' v a

class (HasGetter v a, HasBinding' v a) => HasBinding v a

instance HasBinding' (BindingSource v a b) a

instance HasBinding (BindingSource v a b) a

instance HasBinding' v a => HasBinding' (MappedVariable v WrappedLens a b) b

instance HasBinding v a => HasBinding (MappedVariable v WrappedLens a b) b

mkSource :: (HasGetter v a, HasUpdate v a a, Variable b (Map Int (Binding a))) => v -> IO (BindingSource v a b)
mkSource v = do
    bindings <- newVar $ M.fromList []
    return $ BindingSource v bindings

instance HasGetter (BindingSource v a b) a where
    get (BindingSource v _) = get v

instance HasSetter (BindingSource v a b) a where
    ($=) s@(BindingSource v _) a = do
        previous <- get v
        v $= a
        liftIO $ update s previous

instance HasUpdate (BindingSource v a b) a a

instance (Variable v a, Variable b (Map Int (Binding a))) => Variable (BindingSource v a b) a where
    newVar v = newVar v >>= liftIO . mkSource

update :: BindingSource v a b -> a -> IO ()
update (BindingSource v b) previous = do
    bs <- get b
    a <- get v
    forM_ (M.elems bs) $ \(Binding f) -> f (Just previous) a

instance Bindable (BindingSource v a b) a where
    bind (BindingSource v b) f = do
        bs <- get b
        let i = 1 + maybe 0 (fst . fst) (M.maxViewWithKey bs)
        b $= M.insert i (Binding f) bs
        a <- get v
        liftIO $ f Nothing a
        return $ BindingId i
    unbind (BindingSource _ b) (BindingId i) = b $~ M.delete i

