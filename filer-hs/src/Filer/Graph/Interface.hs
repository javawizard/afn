
module Filer.Graph.Interface where

import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import qualified Data.Set as S

data RefQuery
    = RefHasAttributes AttributeQuery
    | PointsTo ObjectQuery
    | PointsFrom ObjectQuery
    | AndR RefQuery RefQuery
    | OrR RefQuery RefQuery
    | NotR RefQuery
data AttributeQuery
    = HasAttribute String
    | HasValue String ByteString
    | AndA AttributeQuery AttributeQuery
    | OrA AttributeQuery AttributeQuery
    | NotA AttributeQuery
data ObjectQuery 
    = HasIncomingRef RefQuery
    | HasOutgoingRef RefQuery
    | ObjectHasAttributes AttributeQuery
    | AndO ObjectQuery ObjectQuery
    | OrO ObjectQuery ObjectQuery
    | NotO ObjectQuery

-- | An alias for Data.Map.Map String Data.ByteString.Lazy.ByteString used to
-- store object and ref attributes.
type DataMap = M.Map String B.ByteString

-- | A ref is a pointer to an object, along with a type and some associated
-- attributes. Every object has zero or more refs.
data Ref = Ref Hash DataMap
    deriving (Eq, Ord, Read, Show)

-- | An object is an entry in the graph database. It is uniquely identified by
-- its SHA-256 hash, and it contains a type, zero or more attributes, and zero
-- or more refs.
data Object = Object DataMap (S.Set Ref)
    deriving (Eq, Ord, Read, Show)

-- | Graph databases that can be read from.
class ReadDB a where
    -- | Gets the object with a particular hash from the database.
    getObject :: a -> Hash -> IO (Maybe Object)

-- TODO: Should we merge ReadDB and QueryDB at some point?
-- | Graph databases that can be queried. These maintain indexes of some sort
-- to allow for efficient querying.
class QueryDB a where
    -- | Gets the hashes of all objects that match the specified query.
    query :: a -> Query -> IO [Hash]

-- | Graph databases that can be written to.
class WriteDB a where
    -- | Stores an object into the database. The hash of the object will be
    -- returned. If the object already exists, its hash will be returned
    -- without storing it again.
    addObject :: a -> Object -> IO Hash

-- | Graph databases that can be deleted from.
class DeleteDB a where
    -- | Deletes an object from the database, returning True if such an object
    -- existed or False if one didn't.
    deleteObject :: a -> Hash -> IO Bool
    

    
    
    