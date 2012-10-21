
module Filer.Tests.Test1 where

import Filer.Graph.SQL
import Database.HDBC.Sqlite3
import Database.HDBC
import Filer.Graph.Interface
import Filer.Graph.Encoding
import qualified Data.Map as M
import qualified Data.Set as S 

main = do
    sqldb <- connectSqlite3 "/home/jcp/test/sqlite/hashdb.sql"
    db <- connect sqldb
    addObject db (M.fromList [("a", StringValue "b"), ("c", StringValue "d")]) S.empty
    commit sqldb
    putStrLn "It worked!"
