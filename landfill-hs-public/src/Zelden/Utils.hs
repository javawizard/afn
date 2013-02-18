
module Zelden.Utils where

import Control.Monad.Trans.Cont

-- ContT r m a

getCC :: ContT r m (ContT r m a)
getCC = callCC (\c -> let x = c x in return x)

-- | A function that can be used to recover some imperative looping constructs.
-- You typically use it as @loop $ \continue break -> ...@, and call continue
-- and break within @...@ as you normally would in any imperative programming
-- language. Note that break takes an argument, which is the value that the
-- loop should evaluate to when it's called.
loop :: (ContT r m b -> (a -> ContT r m c) -> ContT r m a) -> ContT r m a
loop f = callCC $ \break -> do
    continue <- getCC
    f continue break
