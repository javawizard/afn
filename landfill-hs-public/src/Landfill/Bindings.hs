
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef)

-- | Types that represent containers that can hold a value. The containers are
-- of kind * -> *, with the only type variable being the type of the value held
-- by the container. The container can be modified in the specified monad.
class Monad m => Var v m where
    -- | Creates a new variable with the specified value.
    newVar :: a -> m (v a)
    -- | Gets the current value of the specified variable.
    readVar :: (v a) -> m a
    -- | Sets the value of the specified variable to the specified value.
    writeVar :: (v a) -> a -> m ()
    -- | Modifies the value in the specified variable using the specified
    -- | function.
    modifyVar :: (v a) -> (a -> a) -> m ()

instance Var IORef IO where
    newVar = newIORef
    readVar = readIORef
    writeVar = writeIORef
    modifyVar = modifyIORef

-- | A wrapper around a variable of type v that intercepts write requests to
-- the variable and issues events to everything watching the variable.
data BindVar v a = BindVar { bindings :: v [Binding a],
                             var :: v a }

data Binding a = forall s => Binding (s a) 

instance Monad m => Var BindVar m where
    newVar a = liftM2 BindVar (newVar []) (newVar a)
    readVar v = readVar $ var v
    writeVar = accept
    modifyVar v f = do { a <- readVar v ; writeVar v $ f a }

-- | Things that can produce new values.
class Monad m => Source v m where
    -- | Listen for when a new value is produced and send it to the specified
    -- sink.
    watch :: (Sink s) => v a -> s -> m ()

-- | Things that can accept new values.
class Monad m => Sink v m where
    -- | Accept a new value.
    accept :: v a -> a -> m ()

instance Monad m => Source BindVar m where
    watch v s = do
        currentBindings <- readVar $ bindings v
        let currentBindings = (Binding s):currentBindings
        writeVar (bindings v) currentBindings

instance Monad m => Sink BindVar m where
    accept v a = do
        writeVar $ var v
        currentBindings <- readVar $ bindings v
        forM_ currentBindings \(Binding s) -> do
            accept s a





































 
