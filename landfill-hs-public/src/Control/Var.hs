{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances, FunctionalDependencies, OverlappingInstances #-}
-- I'd really like to do away with the UndecidableInstances bit, but the
-- STM-to-IO Var instance requires it. TODO: Is there a better way to do it?

module Control.Var (
    Var (newVar, readVar, writeVar, modifyVar)
    ) where

import Control.Concurrent.STM
import Control.Monad.Trans
import Data.IORef

-- TODO: Is the fundef necessary?
class Var v m a | v -> a where
    newVar :: a -> m v
    readVar :: v -> m a
    writeVar :: v -> a -> m ()
    modifyVar :: v -> (a -> a) -> m ()

-- | TVars operate in STM
instance Var (TVar a) STM a where
    newVar = newTVar
    readVar = readTVar
    writeVar = writeTVar
    modifyVar = modifyTVar

-- | TVars also operate in IO
instance (MonadIO m) => Var (TVar a) m a where
    newVar = newTVarIO
    readVar = readTVarIO
    writeTVar v a = liftIO $ atomically $ writeTVar v a
    modifyTVar v f = liftIO $ atomically $ modifyTVar v f

-- | IORefs operate in IO
instance (MonadIO m) => Var (IORef a) m a where
    newVar a = liftIO $ newIORef a
    readVar v = liftIO $ readIORef v
    writeVar v a = liftIO $ writeIORef v a
    modifyVar v f = liftIO $ modifyIORef v f





















    