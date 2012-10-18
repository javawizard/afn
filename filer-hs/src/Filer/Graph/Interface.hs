
module Filer.Graph.Interface where

import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import qualified Data.Set as S

-- | A query looking at references between objects
data RefQuery
    -- | Matches a ref if the attribute query matches any of the
    -- ref's attributes
    = RefHasAttributes AttributeQuery
    -- | Matches a ref if it points to an object matching the specified query
    | PointsTo ObjectQuery
    -- | Matches a ref if it points from an object matching the specified query
    | PointsFrom ObjectQuery
    -- | Matches a ref if both queries match
    | AndR RefQuery RefQuery
    -- | Matches a ref if either query matches
    | OrR RefQuery RefQuery
    -- | Matches a ref if the specified query does not match
    | NotR RefQuery
-- | A query looking at attributes on an object or on a ref
data AttributeQuery
    -- | Matches if the specified attribute exists, regardless of its value
    = HasAttribute String
    -- | Matches if the specified attribute exists with the specified value
    | HasValue String ByteString
    -- | Matches if both queries match
    | AndA AttributeQuery AttributeQuery
    -- | Matches if either query matches
    | OrA AttributeQuery AttributeQuery
    -- | Matches if the specified query does not match
    | NotA AttributeQuery
-- | A query looking at objects.
data ObjectQuery
    -- | Matches if the object has a matching ref pointing to it
    = HasIncomingRef RefQuery
    -- | Matches if the object has a matching ref pointing from it
    | HasOutgoingRef RefQuery
    -- | Matches an object if the attribute query matches any of the object's
    -- attributes
    | ObjectHasAttributes AttributeQuery
    -- | Matches an object if both queries match
    | AndO ObjectQuery ObjectQuery
    -- | Matches an object if either query matches
    | OrO ObjectQuery ObjectQuery
    -- | Matches an object if the specified query does not match
    | NotO ObjectQuery

-- | Graph databases that can be read from.
class ReadDB a where
    -- | Gets the object with a particular hash from the database.
    getObject :: a -> Hash -> IO (Maybe Object)

-- TODO: Should we merge ReadDB and QueryDB at some point?
-- | Graph databases that can be queried. These maintain indexes of some sort
-- to allow for efficient querying.
class QueryDB a where
    -- | Gets the hashes of all objects that match the specified query. I might
    -- expand this later to support querying just for refs and attributes in
    -- the future.
    query :: a -> ObjectQuery -> IO [Hash]

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
    

    
    
    