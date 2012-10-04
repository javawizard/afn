
module Filer.Graph.Interface where

-- An Info has a type, which is a string, and a map of additional attributes
-- present on the info. Every object has an info and every ref has an info.
-- For example, folders are stored as an object with an info whose type is
-- "folder", and which has a number of refs, each one's type being "child" and
-- with a single attribute named "name" whose value is the name of the file
-- under which the referred commit is to be stored.
data Info = Info String DataMap
    deriving (Eq, Ord, Read, Show)

-- A ref is a pointer to an object, along with a type and some associated
-- attributes. Every object has zero or more refs.
data Ref = Ref Hash Info
    deriving (Eq, Ord, Read, Show)

-- | An object is an entry in the graph database. It is uniquely identified by
-- its SHA-256 hash, and it contains a type, zero or more attributes, and zero
-- or more refs.
data Object = Object Info [Ref]
    deriving (Eq, Ord, Read, Show)

-- | Graph databases that can be read from.
class ReadDB a where
    -- | Gets the object with a particular hash from the database.
    getObject :: a -> Hash -> IO (Maybe Object)

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
    

    
    
    