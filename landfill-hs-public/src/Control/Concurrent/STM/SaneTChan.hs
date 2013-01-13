
import Control.Concurrent
import Control.Concurrent.STM

-- readTVar, writeTVar, atomically, retry, orElse


data Queue a = Queue (TVar Link)
data Endpoint a = Endpoint (TVar Link)

type Item a = Empty | Item a (Link a)
type Link a = TVar (Item a)

newQueue :: STM (Queue a)
newQueue = do
    link <- newTVar Empty
    chanVar <- newTVar link
    return $ Queue chanVar

writeQueue :: Queue a -> a -> STM ()
writeQueue (Queue chanVar) item = do
    newLink <- newTVar Empty
    let itemWrapper = Item item newLink
    currentLink <- readTVar chanVar
    writeTVar currentLink itemWrapper
    writeTVar chanVar newLink

newEndpoint :: Queue a -> STM (Endpoint a)
newEndpoint (Queue chanVar) = do
    currentLink <- readTVar chanVar
    portVar <- newTVar currentLink
    return $ Endpoint portVar

readEndpoint :: Endpoint a -> STM a
readEndpoint (Endpoint portVar) = do
    currentLink <- readTVar portVar
    currentItem <- readTVar currentLink
    case currentItem of
        Empty -> retry
        Item item nextLink -> do
            writeTVar portVar nextLink
            return item









