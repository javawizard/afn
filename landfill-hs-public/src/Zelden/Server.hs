{-# LANGUAGE ExistentialQuantification, RankNTypes #-} 

module Zelden.Server where

import Zelden.IO
-- import Database.HDBC as DB
import qualified Data.Map as M
import Control.Monad.Trans.Reader


data ConnectionBox = ConnectionBox (forall a. Connection a => a)
data ProtocolBox = ProtocolBox (forall a. Protocol a => a)



type EventCallback = ConnectionBox -> Event -> IO ()


data Manager

type ViewNumber

data BroadcastManager k a s = BroadcastManager s (TVar (M.Map k (Queue a, TVar s))

newBroadcastManager :: s -> STM (BroadcastManager k a s)
newBroadcastManager defaultState = liftM (BroadcastManager defaultState) $ newTVar M.empty

getOrCreateQueue :: BroadcastManager k a s -> k -> (Queue a, TVar s)
getOrCreateQueue manager key = do
    let (BroadcastManager defaultState mVar) = manager
    m <- readTVar mVar
    qs <- liftM (lookup key) m
    case qs of
        Just t -> t            
        Nothing -> do
            q <- newQueue
            s <- newTVar defaultState
            modifyTVar mVar $ insert key (q, s)
            return (q, s)

attachTo :: BroadcastManager k a s -> k -> STM (Endpoint a, s)
attachTo manager key = do
    (q, s) <- getOrCreateQueue manager key
    state <- readTVar s
    endpoint <- newEndpoint q
    return (endpoint, state)

broadcastTo :: BroadcastManager k a s -> k -> a -> s -> STM ()
broadcastTo manager key value state = do
    (q, s) <- getOrCreateQueue manager key
    writeQueue q value
    writeTVar s state

type ZBroadcastManager = BroadcastManager ViewID BroadcastEvent MessageID
    




-- type DBM a = ReaderT DB.Connection IO a

-- type DBAction = forall a. DBAction (DBM a) (TMVar a)

-- closeDB :: DBM ()
-- closeDB = ask >>= lift . DB.close














    