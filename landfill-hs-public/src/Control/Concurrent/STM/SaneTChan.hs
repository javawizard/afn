
-- | A sane TChan-like library. It's quite similar to
-- 'Control.Concurrent.STM.TChan', but it separates the read end and the write
-- end of the channel into distinct types. This elegantly solves the problem
-- that was solved in a much more hackish way by 
-- 'Control.Concurrent.STM.TChan.newBroadcastTChan'.
module Control.Concurrent.STM.SaneTChan (Queue, Endpoint, newQueue,
    newEndpoint, writeQueue, readEndpoint, tryReadEndpoint, peekEndpoint,
    tryPeekEndpoint, unGetEndpoint, isEmptyEndpoint, cloneEndpoint) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (liftM)

-- readTVar, writeTVar, atomically, retry, orElse

-- | A queue that can be written to.
data Queue a = Queue (TVar (Link a))
-- | An endpoint that can be used for reading items written to a queue.
data Endpoint a = Endpoint (TVar (Link a))

data Item a = Empty | Item a (Link a)
type Link a = TVar (Item a)

-- | Creates a new 'Queue'.
newQueue :: STM (Queue a)
newQueue = do
    link <- newTVar Empty
    chanVar <- newTVar link
    return $ Queue chanVar

-- | Write an item to a queue. The item will then be available for reading from
-- any endpoints previously created with newEndpoint.
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

-- | Read the next value from the specified endpoint, retrying if no value is
-- currently available.
readEndpoint :: Endpoint a -> STM a
readEndpoint (Endpoint endpointVar) = do
    currentLink <- readTVar endpointVar
    currentItem <- readTVar currentLink
    case currentItem of
        Empty -> retry
        Item item nextLink -> do
            writeTVar endpointVar nextLink
            return item

-- | A version of 'readEndpoint' which does not retry. Instead it returns
-- @Nothing@ if no value is available.
tryReadEndpoint :: Endpoint a -> STM (Maybe a)
tryReadEndpoint e = liftM Just (readEndpoint e) `orElse` return Nothing

-- | Get the next value from the @Endpoint@ without removing it, retrying if
-- the endpoint is empty.
peekEndpoint :: Endpoint a -> STM a
peekEndpoint (Endpoint endpointVar) = do
    currentLink <- readTVar endpointVar
    currentItem <- readTVar currentLink
    case currentItem of
        Empty -> retry
        Item item _ -> return item

-- | A version of 'peekEndpoint' which does not retry. Instead it returns
-- @Nothing@ if no value is available.
tryPeekEndpoint :: Endpoint a -> STM (Maybe a)
tryPeekEndpoint e = liftM Just (peekEndpoint e) `orElse` return Nothing

-- | Put a value back onto an endpoint, where it will be the next item read.
unGetEndpoint :: Endpoint a -> a -> STM ()
unGetEndpoint (Endpoint endpointVar) item = do
    currentLink <- readTVar endpointVar
    newLink <- newTVar (Item item currentLink)
    writeTVar endpointVar newLink

-- | Returns 'True' if the supplied 'Endpoint' is empty.

isEmptyEndpoint :: Endpoint a -> STM Bool
isEmptyEndpoint e = peekEndpoint e >> return True `orElse` return False

-- | Clone an 'Endpoint': similar to 'newEndpoint', but the new endpoint
-- starts with the same content available as the original endpoint.
cloneEndpoint :: Endpoint a -> STM (Endpoint a)
cloneEndpoint (Endpoint endpointVar) = do
    currentLink <- readTVar endpointVar
    newEndpointVar <- newTVar currentLink
    return $ Endpoint newEndpointVar

 
     









