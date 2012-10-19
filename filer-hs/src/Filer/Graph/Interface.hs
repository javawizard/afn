
module Filer.Graph.Interface where

import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import qualified Data.Set as S
import Filer.Graph.Encoding (makeHash, Value, DataMap)

data ValueQuery
    = IntValue IntQuery
    | BoolValue BoolQuery
    | StringValue StringQuery
    | BinaryValue BinaryQuery
    | AndV ValueQuery ValueQuery
    | OrV ValueQuery ValueQuery
    | NotV ValueQuery ValueQuery
    | AnyValue

data IntQuery
    = IntGreaterThan Integer
    | IntLessThan Integer
    | IntEqualTo Integer
    | AnyInt

data BoolQuery
    = BoolEqualTo Bool
    | AnyBool

data StringQuery
    = StringEqualTo String
    -- TODO: Add additional things for seeing if the string in question is
    -- contained within an attribute's value or things like that, or maybe even
    -- support regexes
    | AnyString

data BinaryQuery
    = BinaryEqualTo B.ByteString
    -- TODO: Add length queries, so that we can search for attributes with
    -- large values
    | AnyBinary

data RefQuery
    = RefHasAttributes AttributeQuery
    | PointsTo ObjectQuery
    | PointsFrom ObjectQuery
    | AndR RefQuery RefQuery
    | OrR RefQuery RefQuery
    | NotR RefQuery

data AttributeQuery
    = HasAttribute String ValueQuery
    | AndA AttributeQuery AttributeQuery
    | OrA AttributeQuery AttributeQuery
    | NotA AttributeQuery

data ObjectQuery
    = HasIncomingRef RefQuery
    | HasOutgoingRef RefQuery
    | ObjectHasAttributes AttributeQuery
    | HashIs Hash
    | AndO ObjectQuery ObjectQuery
    | OrO ObjectQuery ObjectQuery
    | NotO ObjectQuery

-- | Graph databases that can be read from.
class ReadDB a where
    getObjectAttributes :: a -> Hash -> IO (Maybe DataMap)
    getObjectRefs :: a -> Hash -> IO (Maybe S.Set (Hash, DataMap))
    getAllObjects :: a -> [Hash]

-- TODO: Should we merge ReadDB and QueryDB at some point?
-- | Graph databases that can be queried. These maintain indexes of some sort
-- to allow for efficient querying.
class QueryDB a where
    -- | Gets the hashes of all objects that match the specified query. I might
    -- expand this later to support querying just for refs and attributes in
    -- the future.
    runObjectQuery :: a -> ObjectQuery -> IO [Hash]

-- | Graph databases that can be written to.
class WriteDB a where
    -- | Stores an object into the database. The hash of the object will be
    -- returned. If the object already exists, its hash will be returned
    -- without storing it again.
    addObject :: a -> (DataMap, S.Set (Hash, DataMap)) -> IO Hash

-- | Graph databases that can be deleted from.
class DeleteDB a where
    -- | Deletes an object from the database, returning True if such an object
    -- existed or False if one didn't.
    deleteObject :: a -> Hash -> IO Bool
    

    
    
    