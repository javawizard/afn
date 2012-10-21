
module Filer.Graph.SQL where

import Filer.Graph.Interface
import Database.HDBC (IConnection, run, commit, getTables)
import Filer.

runInitialStatements :: IConnection c => c -> IO ()
runInitialStatements conn = do
    let r s = run c s [] 
    r "create table objects (id integer auto_increment, hash text)"
    r "create index objects_id_hash on objects (id, hash)"
    r "create index objects_hash_id on objects (hash, id)"
    r "create table refs (id integer auto_increment, source integer, target integer)"
    r "create index refs_id_source on refs (id, source, target)"
    r "create index refs_id_target on refs (id, target, source)"
    r "create index refs_source_id on refs (source, id)"
    r "create inex refs_target_id on refs (target, id)"
    r "create table attributes (id integer, sourcetype integer, " ++
        "name text, intvalue integer, stringvalue text, boolvalue integer, " ++
        "blobvalue blob)"
    let attrIndex name = "create index attributes_sourcetype_name_" ++ name ++
        " on attributes (sourcetype, name, " ++ name ++ ")"
    r $ attrIndex "intvalue"
    r $ attrIndex "stringvalue"
    r $ attrIndex "boolvalue"
    -- We're not doing similar for blobvalue as from what I've read a decent
    -- majority of databases don't support indexes on blobs

data DB = forall a. IConnection a => DB a

connect :: IConnection a => a -> IO DB
connect c = do
    -- If the tables haven't been created, create them. We might want to create
    -- some sort of version table in the future so that we can detect if we're
    -- using a newer schema than the one we created here, and migrate
    -- everything to a new set of tables as needed.
    tableNames = getTables c
    when (not $ elem "refs" tableNames) $ runInitialStatements c
    return $ DB c

instance ReadDB DB a where
    ...

instance QueryDB DB where
    ...

instance WriteDB DB where
    ...

instance DeleteDB DB where
    deleteObject (DB c) hash = do
        






























