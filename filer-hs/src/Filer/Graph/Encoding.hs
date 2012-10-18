
module Encoding where

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