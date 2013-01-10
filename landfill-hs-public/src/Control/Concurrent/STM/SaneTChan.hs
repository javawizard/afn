
import Control.Concurrent
import Control.Concurrent.STM

-- readTVar, writeTVar, atomically, retry, orElse


data TChan a = TChan (TVar Link)
data TPort a = TPort (TVar Link)

type Item a = Empty | Item a (Link a)
type Link a = TVar (Item a)

newTChan :: STM (TChan a)
newTChan = do
    link <- newTVar Empty
    chanVar <- newTVar link
    return $ TChan chanVar

writeTChan :: TChan a -> a -> STM ()
writeTChan (TChan chanVar) item = do
    newLink <- newTVar Empty
    let itemWrapper = Item item newLink
    currentLink <- readTVar chanVar
    writeTVar currentLink itemWrapper
    writeTVar chanVar newLink

newTPort :: TChan a -> STM (TPort a)
newTPort (TChan chanVar) = do
    currentLink <- readTVar chanVar
    portVar <- newTVar currentLink
    return $ TPort portVar

readTPort :: TPort a -> STM a
readTPort (TPort portVar) = do
    currentLink <- readTVar portVar
    currentItem <- readTVar currentLink
    case currentItem of
        Empty -> retry
        Item item nextLink -> do
            writeTVar portVar nextLink
            return item









