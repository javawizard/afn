
module Control.Concurrent.STM.Utils where

import Control.Concurrent.STM.TVar (readTVar, atomically, registerDelay)

-- | Runs the specified STM action, which may retry. If it retries but, for
-- whatever reason, it is unable to actually produce a value by the time the
-- specified delay (in nanoseconds) is up, Nothing will instead be returned.
-- This is basically just a thin wrapper around
-- `Control.Concurrent.STM.TVar.registerDelay`.
runTimed :: Int -> STM a -> IO (Maybe a)
runTimed delay action = do
    var <- registerDelay delay
    atomically liftM Just action `orElse` do
        timedOut <- readTVar var
        if timedOut then return Nothing else retry
