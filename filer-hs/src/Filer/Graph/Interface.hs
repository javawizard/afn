
module Filer.Graph.Interface where

import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import qualified Data.Set as S
import Filer.Graph.Encoding (Value, DataMap, fromPretty, Pretty(..), PrettyObject)
import Filer.Graph.Query
import Filer.Hash (Hash)


-- | Graph databases that can be read from.
class ReadDB a where
    getObject :: a -> Hash -> IO (Maybe (DataMap, S.Set (Hash, DataMap)))
    getAllHashes :: a -> IO [Hash]
    getObjectCount :: a -> IO Integer

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
    addObject :: a -> DataMap -> S.Set (Hash, DataMap) -> IO Hash

addObject' :: (WriteDB a) => a -> (DataMap, S.Set (Hash, DataMap)) -> IO Hash
addObject' db = uncurry (addObject db)

addPrettyObject :: (WriteDB a) => a -> PrettyObject -> IO Hash
addPrettyObject db object = addObject' db $ fromPretty object

-- | Graph databases that can be deleted from.
class DeleteDB a where
    -- | Deletes an object from the database, returning True if such an object
    -- existed or False if one didn't.
    deleteObject :: a -> Hash -> IO Bool
    

    
    
    