
module Filer.Graph.SQL where

import Filer.Graph.Interface
import Database.HDBC (IConnection, run, commit, getTables)

data DB = forall a. IConnection a => DB a

connect :: IConnection a => a -> IO DB
connect = do
    

instance ReadDB DB a where
    ...

instance QueryDB DB where
    ...

instance WriteDB DB where
    ...

instance DeleteDB DB where
    ...