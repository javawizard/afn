
module Filer.Tests.Test1 where

import Filer.Graph.SQL
import Database.HDBC.Sqlite3
import Database.HDBC
import Filer.Graph.Interface
import Filer.Graph.Encoding
import qualified Data.Map as M
import qualified Data.Set as S 
import Filer.Hash (toHex)
import Control.Monad (forM, forM_)
import Filer.Graph.Encoding (toPretty, toPretty', fromPretty, Pretty ((:=)))
import Data.Maybe (fromJust)
import Filer.Graph.Query

sampleQuery = ObjectHasAttribute "f" $ IntValueQuery $ IntEqualTo 45

main = do
    putStrLn "Connecting to sqlite3 database..."
    sqldb <- connectSqlite3 "/home/jcp/test/sqlite/hashdb.sql"
    putStrLn "Opening a HashDB connected to the sqlite3 database..."
    db <- connect sqldb
    putStrLn "Writing two objects..."
    hash1 <- addPrettyObject db $ ["a" := StringValue "b", "c" := StringValue "d", "e" := StringValue "f"] := []
    hash2 <- addPrettyObject db $ ["e" := IntValue 42] := [toHex hash1 := ["f" := IntValue 45]]
    putStrLn "Two objects written. Number of objects in the database:"
    getObjectCount db >>= putStrLn . show
    putStrLn "About to read all objects back..."
    hashes <- getAllHashes db
    objects <- forM hashes $ getObject db
    forM_ (zip hashes objects) $ \(hash, object) -> putStrLn $ toHex hash ++ ": " ++ (show $ toPretty' $ fromJust object)
    putStrLn $ "Sample query's SQL is " ++ (show $ tokensToSql $ fObjectQueryToSql sampleQuery)
    putStrLn "About to commit..." 
    commit sqldb
    putStrLn "Done!"
