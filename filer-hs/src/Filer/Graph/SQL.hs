{-# LANGUAGE ExistentialQuantification #-}

module Filer.Graph.SQL where

import Filer.Graph.Interface
import Database.HDBC (IConnection, run, commit, getTables, quickQuery', toSql,
    SqlValue(SqlNull), prepare, executeMany, fromSql)
import Filer.Hash (Hash, toHex, fromHex)
import Filer.Graph.Encoding (hashObject, Value(..), DataMap)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad (forM, forM_, liftM, when)
import Filer.Graph.Query

isNull = (== SqlNull)

isNotNull = (/= SqlNull)

encodeValue :: Value -> SqlValue
encodeValue (IntValue i)    = toSql i
encodeValue (StringValue s) = toSql s
encodeValue (BinaryValue b) = toSql b
-- Bools encode as integers, 0 for False and 1 for True
encodeValue (BoolValue b)   = toSql $ fromEnum b

runInitialStatements :: IConnection c => c -> IO ()
runInitialStatements c = do
    let r :: String -> IO Integer; r s = run c s [] 
    r "create table objects (id integer primary key autoincrement, hash text)"
    r "create index objects_id_hash on objects (id, hash)"
    r "create index objects_hash_id on objects (hash, id)"
    r "create unique index objects_id on objects (id)"
    r "create unique index objects_hash on objects(hash)"
    r "create table refs (id integer primary key autoincrement, source integer, target integer)"
    r "create unique index refs_id on refs (id)"
    r "create index refs_id_source on refs (id, source, target)"
    r "create index refs_id_target on refs (id, target, source)"
    r "create index refs_source_id on refs (source, id)"
    r "create index refs_target_id on refs (target, id)"
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

instance ReadDB DB where
    getObject (DB c) hash = do
        -- See if the object exists, and get its id
        objectIdQuery <- quickQuery' c "select id from objects where hash = ?" [toSql $ toHex hash]
        case objectIdQuery of
            [[objectIdSql]] -> do
                -- It does. Get its attributes.
                let objectId = fromSql objectIdSql :: Integer
                objectAttributes <- readAttributes c 1 objectId
                -- Then get its refs
                refQuery <- quickQuery' c "select id, target, (select hash from objects where id = target) from refs where source = ?" [toSql objectId]
                refList <- forM refQuery $ \[refId, refTargetId, refTargetHash] -> do
                    -- Get this ref's attributes
                    refAttributes <- readAttributes c 2 $ fromSql refId
                    -- Return the ref
                    return (fromHex $ fromSql refTargetHash, refAttributes)
                -- Return the object
                return $ Just (objectAttributes, S.fromList refList)
            _ -> return Nothing -- Doesn't exist
    -- This is as simple as querying for all the hashes in the database, then
    -- flattening them out into a list of strings, then converting them all to
    -- hashes.
    getAllHashes (DB c) = liftM (map (fromHex . fromSql) . concat) $ quickQuery' c "select hash from objects" []
    getObjectCount (DB c) = liftM ((fromSql :: SqlValue -> Integer) . head . head) $ quickQuery' c "select count(id) from objects" []

-- Now we have the glorious part of this whole thing dedicated to converting
-- HashDB queries to SQL queries.

data SqlToken = Param SqlValue | Text String

param :: (Convertible a SqlValue) => a -> SqlToken
param a = Param $ toSql a

valueQueryToSql :: ValueQuery -> String -> [SqlToken]
valueQueryToSql (IntValueQuery v) name = [Text "name = ", param name] ++ intQueryToSql v
valueQueryToSql (BoolValueQuery v) name = [Text "name = ", param name] ++ boolQueryToSql v
valueQueryToSql (StringValueQuery v) name = [Text "name = ", param name] ++ stringQueryToSql v
valueQueryToSql (BinaryValueQuery v) name = [Text "name = ", param name] ++ binaryQueryToSql v
valueQueryToSql (AndV a b) name = [Text "("] ++ (valueQueryToSql a name) ++ [Text ") and ("] ++ (valueQueryToSql b name) ++ [Text ")"]
valueQueryToSql (OrV a b) name = [Text "("] ++ (valueQueryToSql a name) ++ [Text ") or ("] ++ (valueQueryToSql b name) ++ [Text ")"]
valueQueryToSql (NotV a) name = [Text "(not ("] ++ (valueQueryToSql a name) ++ [Text "))"]
valueQueryToSql AnyValue name = [Text "name = ", param name]

intQueryToSql :: IntQuery -> [SqlToken]
intQueryToSql a = [Text " and intvalue is not null"] ++ (intQueryToSql' a)

intQueryToSql' :: IntQuery -> [SqlToken]
intQueryToSql' (IntGreaterThan i) = [Text " and intvalue > ", param i]
intQueryToSql' (IntGreaterOrEqual i) = [Text " and intvalue >= ", param i]
intQueryToSql' (IntLessThan i) = [Text " and intvalue < ", param i]
intQueryToSql' (IntLessOrEqual i) = [Text " and intvalue <= ", param i]
intQueryToSql' (IntInRange a b) = [Text " and intvalue >= ", param a, Text " and intvalue <= ", param b]
intQueryToSql' (IntEqualTo i) = [Text " and intvalue = ", param i]
intQueryToSql' AnyInt = []

boolQueryToSql :: BoolQuery -> [SqlToken]
boolQueryToSql b = [Text " and boolvalue is not null"] ++ boolQueryToSql' b

boolQueryToSql' :: BoolQuery -> [SqlToken]
boolQueryToSql' (BoolEqualTo b) = [Text " and boolvalue = ", param $ fromEnum b]
boolQueryToSql' AnyBool = []

stringQueryToSql :: StringQuery -> [SqlToken]
stringQueryToSql s = [Text " and stringvalue is not null"] ++ stringQueryToSql' s

stringQueryToSql' :: StringQuery -> [SqlToken]
stringQueryToSql' (StringEqualTo s) = [Text " and stringvalue = ", param s]
stringQueryToSql' AnyString = []

binaryQueryToSql :: BinaryQuery -> [SqlToken]
binaryQueryToSql b = [Text " and binaryvalue is not null"] ++ binaryQueryToSql' b

binaryQueryToSql' :: BinaryQuery -> [SqlToken]
binaryQueryToSql' (BinaryEqualTo b) = [Text " and binaryvalue = ", param b]
binaryQueryToSql' AnyBinary = []

instance QueryDB DB where
    runObjectQuery = undefined

readAttributes :: IConnection c => c -> Integer -> Integer -> IO DataMap
readAttributes c sourceType sourceId = do
    queryResults <- quickQuery' c "select name, intvalue, stringvalue, boolvalue, binaryvalue from attributes where sourcetype = ? and id = ?" [toSql sourceType, toSql sourceId]
    return $ M.fromList $ flip map queryResults $ \[name, intValue, stringValue, boolValue, binaryValue] -> let n = (fromSql name :: String) in case () of
        _ | isNotNull intValue -> (n, IntValue $ fromSql intValue)
        _ | isNotNull stringValue -> (n, StringValue $ fromSql stringValue)
        _ | isNotNull boolValue -> (n, BoolValue $ toEnum $ fromSql boolValue)
        _ | isNotNull binaryValue -> (n, BinaryValue $ fromSql binaryValue)

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
            [[_]] -> return objectHash
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
                    run c "insert into refs (source, target) values (?, ?)" [toSql objectId, toSql targetId]
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






























