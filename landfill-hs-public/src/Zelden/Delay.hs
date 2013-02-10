
module Zelden.Delay where

import GHC.Event as E
import Control.Concurrent.STM

-- TODO: Replace with Control.Concurrent.STM.TVar.registerDelay, which is what
-- I wrote this to emulate (I knew it existed but had somehow overlooked it,
-- and mistakenly concluded that it had been removed.)

-- | Run the specified IO action after the specified number of microseconds.
registerTimeout :: Int -> IO () -> IO ()
registerTimeout delay action = do
    eventManager <- E.getSystemEventManager
    E.registerTimeout eventManager delay action

-- | Return a new TVar whose value is False. After the specified number of
-- microseconds, the TVar's value will be set to True.
makeTimeout :: Int -> IO (TVar Bool)
makeTimeout delay = do
    var <- newTVarIO False
    registerTimeout delay $ writeTVarIO var True
    return var

