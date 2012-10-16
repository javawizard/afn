
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef)

-- | Types that represent containers that can hold a value. Values held within
-- the containers can be changed in the IO monad. This will eventually be
-- generalized to work with any monad, probably by changing this class to
-- Var v m and then having instances such as Var IORef IO and Var TVar STM.
class Var v where
    -- | Creates a new variable with the specified value.
    newVar :: a -> IO (v a)
    -- | Gets the current value of the specified variable.
    readVar :: (v a) -> IO a
    -- | Sets the value of the specified variable to the specified value.
    writeVar :: (v a) -> a -> IO ()
    -- | Modifies the value in the specified variable using the specified
    -- | function.
    modifyVar :: (v a) -> (a -> a) -> IO ()

instance Var IORef where
    newVar = newIORef
    readVar = readIORef
    writeVar = writeIORef
    modifyVar = modifyIORef

-- | A wrapper around a variable of type v that intercepts write requests to
-- the variable and issues events to everything watching the variable.
data BindVar v a = BindVar { bindings :: v [Binding a],
                             var :: v a }

data Binding a = forall s => Binding (s a) 

instance Var BindVar where
    newVar a = liftM2 BindVar (newVar []) (newVar a)
    readVar v = readVar $ var v
    writeVar = accept
    modifyVar v f = do { a <- readVar v ; writeVar v $ f a }

-- | Things that can produce new values.
class Source v where
    -- | Listen for when a new value is produced and send it to the specified
    -- sink.
    watch :: Sink s => v a -> s -> IO ()

-- | Things that can accept new values.
class Sink v where
    -- | Accept a new value.
    accept :: v a -> a -> IO ()

instance Source BindVar where
    watch v s = do
        currentBindings <- readVar $ bindings v
        let currentBindings = (Binding s):currentBindings
        writeVar (bindings v) currentBindings

instance Sink BindVar where
    accept v a = do
        writeVar $ var v
        currentBindings <- readVar $ bindings v
        forM_ currentBindings \(Binding s) -> do
            accept s a





































 
