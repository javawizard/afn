
module Filer.Tests.Test1 where

import Filer.Graph.SQL
import Database.HDBC.Sqlite3
import Database.HDBC
import Filer.Graph.Interface
import Filer.Graph.Encoding
import qualified Data.Map as M
import qualified Data.Set as S 
import Filer.Hash (toHex)

main = do
    putStrLn "Connecting to sqlite3 database..."
    sqldb <- connectSqlite3 "/home/jcp/test/sqlite/hashdb.sql"
    putStrLn "Opening a HashDB connected to the sqlite3 database..."
    db <- connect sqldb
    putStrLn "Writing two objects..."
    hash1 <- addObject db (M.fromList [("a", StringValue "b"), ("c", StringValue "d")]) S.empty
    hash2 <- addObject db (M.fromList [("e", IntValue 42)]) $ S.fromList [(hash1, M.fromList [("f", IntValue 45)])]
    putStrLn "Two objects written. Number of objects in the database:"
    getObjectCount db >>= putStrLn . show
    putStrLn "About to read all objects back..."
    getAllHashes db >>= mapM_ (\h -> getObject db h >>= putStrLn . ((toHex h ++ ": ") ++) . show)
    putStrLn "All objects read. About to commit..." 
    commit sqldb
    putStrLn "Done!"
