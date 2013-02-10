{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
-- I'd really like to do away with the UndecidableInstances bit, but the
-- STM-to-IO Var instance requires it. TODO: Is there a better way to do it?

module Control.Var (
    Var (newVar, readVar, writeVar, modifyVar)
    ) where

import Control.Concurrent.STM
import Control.Monad.Trans
import Data.IORef

class Var v m a where
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

-- | Any var (including TVars) that operates in STM also operates in IO by
-- wrapping each action with a call to @atomically@
instance (MonadIO m, Var v STM a) => Var v m a where
    newVar a = liftIO $ atomically $ newVar a
    readVar v = liftIO $ atomically $ readVar v
    writeVar v a = liftIO $ atomically $ writeVar v a
    modifyVar v f = liftIO $ atomically $ modifyVar v f

-- | IORefs operate in IO
instance (MonadIO m) => Var (IORef a) m a where
    newVar a = liftIO $ newIORef a
    readVar v = liftIO $ readIORef v
    writeVar v a = liftIO $ writeIORef v a
    modifyVar v f = liftIO $ modifyIORef v f





















    