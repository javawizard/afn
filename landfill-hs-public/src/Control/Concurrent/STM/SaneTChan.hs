
-- | A sane TChan-like library. It's quite similar to
-- Control.Concurrent.STM.TChan, but it separates the read end and the write
-- end of the channel into distinct types.
module Control.Concurrent.STM.SaneTChan where

import Control.Concurrent
import Control.Concurrent.STM

-- readTVar, writeTVar, atomically, retry, orElse

-- | A queue that can be written to.
data Queue a = Queue (TVar (Link a))
-- | An endpoint that can be used for reading items written to a queue.
data Endpoint a = Endpoint (TVar (Link a))

data Item a = Empty | Item a (Link a)
type Link a = TVar (Item a)

-- | Creates a new queue.
newQueue :: STM (Queue a)
newQueue = do
    link <- newTVar Empty
    chanVar <- newTVar link
    return $ Queue chanVar

-- | Writes the specified item to the specified queue. The item will then be
-- available for reading from any endpoints previously created with
-- newEndpoint.
writeQueue :: Queue a -> a -> STM ()
writeQueue (Queue chanVar) item = do
    newLink <- newTVar Empty
    let itemWrapper = Item item newLink
    currentLink <- readTVar chanVar
    writeTVar currentLink itemWrapper
    writeTVar chanVar newLink

-- | Creates a new endpoint that reads from the specified queue. The endpoint
-- initially starts out empty; items will become available once writeQueue is
-- called to write items to the queue.
newEndpoint :: Queue a -> STM (Endpoint a)
newEndpoint (Queue chanVar) = do
    currentLink <- readTVar chanVar
    endpointVar <- newTVar currentLink
    return $ Endpoint endpointVar

-- | Reads the next value from the specified endpoint, retrying if no value is
-- currently available.
readEndpoint :: Endpoint a -> STM a
readEndpoint (Endpoint portVar) = do
    currentLink <- readTVar portVar
    currentItem <- readTVar currentLink
    case currentItem of
        Empty -> retry
        Item item nextLink -> do
            writeTVar portVar nextLink
            return item









