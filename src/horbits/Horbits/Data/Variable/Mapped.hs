{-# LANGUAGE Rank2Types #-}

module Horbits.Data.Variable.Mapped(MappedVariable, mapVariable) where

import           Control.Applicative
import           Control.Lens
import           Data.Binding.Simple

data MappedVariable v a b = MappedVariable (v a) (Lens' a b)


instance Variable v => Variable (MappedVariable v a) where
    newVar = error "Cannot newVar a MappedBindingSource, sorry." -- urgh
    readVar (MappedVariable src l) = view l <$> readVar src
    writeVar (MappedVariable src l) v = modifyVar src $ l .~ v
    modifyVar (MappedVariable src l) f = modifyVar src $ l %~ f
    modifyVar' s f = do
                          a <- readVar s
                          let (a', b) = f a
                          writeVar s a'
                          return b

instance Bindable v => Bindable (MappedVariable v a) where
    bind (MappedVariable src l) f = bind src (views l f)

mapVariable :: Variable v => Lens' a b -> v a -> MappedVariable v a b
mapVariable = flip MappedVariable

