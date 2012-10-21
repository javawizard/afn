{-# LANGUAGE ExistentialQuantification #-}

module Filer.Graph.SQL where

import Filer.Graph.Interface
import Database.HDBC (IConnection, run, commit, getTables, quickQuery', toSql,
    SqlValue(SqlNull), prepare, executeMany, fromSql)
import Filer.Hash (Hash, toHex, fromHex)
import Filer.Graph.Encoding (hashObject, Value(..), DataMap)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad (forM_, when)

encodeValue :: Value -> SqlValue
encodeValue (IntValue i)    = toSql i
encodeValue (StringValue s) = toSql s
encodeValue (BinaryValue b) = toSql b
-- Bools encode as integers, 0 for False and 1 for True
encodeValue (BoolValue b)   = toSql $ fromEnum b

runInitialStatements :: IConnection c => c -> IO ()
runInitialStatements c = do
    let r :: String -> IO Integer; r s = run c s [] 
    r "create table objects (id integer auto_increment, hash text)"
    r "create index objects_id_hash on objects (id, hash)"
    r "create index objects_hash_id on objects (hash, id)"
    r "create unique index objects_id on objects (id)"
    r "create unique index objects_hash on objects(hash)"
    r "create table refs (id integer auto_increment, source integer, target integer)"
    r "create unique index refs_id on refs (id)"
    r "create index refs_id_source on refs (id, source, target)"
    r "create index refs_id_target on refs (id, target, source)"
    r "create index refs_source_id on refs (source, id)"
    r "create inex refs_target_id on refs (target, id)"
    r ("create table attributes (id integer, sourcetype integer, " ++
        "name text, intvalue integer, stringvalue text, boolvalue integer, " ++
        "binaryvalue blob)")
    r "create index attributes_sourcetype_id on attributes (sourcetype, id)"
    let {attrIndex name = ("create index attributes_sourcetype_name_" ++ name ++
        " on attributes (sourcetype, name, " ++ name ++ ")")}
    r $ attrIndex "intvalue"
    r $ attrIndex "stringvalue"
    r $ attrIndex "boolvalue"
    return ()
    -- We're not doing similar for binaryvalue as from what I've read a decent
    -- majority of databases don't support indexes on blobs

data DB = forall a. IConnection a => DB a

connect :: IConnection a => a -> IO DB
connect c = do
    -- If the tables haven't been created, create them. We might want to create
    -- some sort of version table in the future so that we can detect if we're
    -- using a newer schema than the one we created here, and migrate
    -- everything to a new set of tables as needed.
    tableNames <- getTables c
    when (not $ elem "refs" tableNames) $ runInitialStatements c
    return $ DB c

-- instance ReadDB DB a where
--     ...

-- instance QueryDB DB where
--     ...

insertAttributes :: IConnection c => c -> Integer -> Integer -> DataMap -> IO ()
insertAttributes c sourceType sourceId attributes = do
    statement <- prepare c "insert into attributes (sourcetype, id, name, intvalue, stringvalue, boolvalue, binaryvalue) values (?, ?, ?, ?, ?, ?, ?)"
    executeMany statement $ flip map (M.toList attributes) $ \(k, v) -> [toSql sourceType, toSql sourceId, toSql k] ++ case v of
        (IntValue i)    -> [toSql i, SqlNull, SqlNull, SqlNull]
        (StringValue s) -> [SqlNull, toSql s, SqlNull, SqlNull]
        (BoolValue b)   -> [SqlNull, SqlNull, toSql $ fromEnum b, SqlNull]
        (BinaryValue b) -> [SqlNull, SqlNull, SqlNull, toSql b]
    -- TODO: Not sure if this is needed or if executeMany returns IO ()
    return ()

instance WriteDB DB where
    addObject (DB c) attributeMap refSet = do
        -- Hash the object
        let objectHash = hashObject attributeMap refSet
        -- See if it already exists
        existingObjectQuery <- quickQuery' c "select id from objects where hash = ?" [toSql $ toHex objectHash]
        case existingObjectQuery of
            -- If it already exists, just return its hash
            [[]] -> return objectHash
            _    -> do
                -- Doesn't exist, so we need to create it. First we'll insert
                -- the object into the objects table.
                run c "insert into objects (hash) values (?)" [toSql $ toHex objectHash]
                -- Then we query for the id it received. TODO: See if there's
                -- a more efficient way to do this without querying for the id.
                [[objectIdSql]] <- quickQuery' c "select max(id) from objects" []
                let objectId = fromSql objectIdSql :: Integer
                -- Then we insert the object's attributes
                insertAttributes c 1 objectId attributeMap
                -- Then we iterate over each of the object's refs
                forM_ (S.toList refSet) $ \(refTargetHash, refAttributes) -> do
                    -- Look up the ref's target's id
                    targetQuery <- quickQuery' c "select id from objects where hash = ?" [toSql $ toHex refTargetHash]
                    targetId <- case targetQuery of
                        [[i]] -> return i
                        _     -> error ("Ref target " ++ (toHex refTargetHash) ++ " was not found. Support for " ++
                                     "inserting objects pointing to objects that do not exist yet may be added in the future.")
                    -- Insert the ref
                    run c "insert into refs (source, dest) values (?, ?)" [toSql objectId, toSql targetId]
                    -- Get the ref's id
                    [[refIdSql]] <- quickQuery' c "select max(id) from refs" []
                    let refId = fromSql refIdSql :: Integer
                    -- Insert the ref's attributes
                    insertAttributes c 2 refId refAttributes
                -- And that's it!
                return objectHash

instance DeleteDB DB where
    deleteObject (DB c) hash = do
        objectIdQuery <- quickQuery' c "select id from objects where hash = ?" [toSql $ toHex hash]
        case objectIdQuery of
            -- If no such object exists, we're done
            [[]]            -> return False
            [[objectIdSql]] -> do
                -- It does exist, so delete it.
                let objectId = (fromSql objectIdSql :: Integer)
                -- Delete the object
                run c "delete from objects where id = ?" [toSql objectId]
                -- Delete the object's attributes
                run c "delete from attributes where sourcetype = 1 and id = ?" [toSql objectId]
                -- Delete the object's refs' attributes
                run c "delete from attributes where sourcetype = 2 and id in (select id from refs where source = ?)" [toSql objectId]
                -- Delete the object's refs
                run c "delete from refs where source = ?" [toSql objectId]
                -- And we're done!
                return True






























