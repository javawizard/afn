
module Encoding where
import Data.Binary (Binary, get, put)
import Data.Set (Set)

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

-- We're using instances of Binary to encode/decode things. This may or may
-- not be a good idea as I'm not sure how encoding plays with older versions of
-- Haskell, and I'd like to be able to separately define the encoding for 
-- objects so that things written in langauges other than Haskell can read and
-- write databases. But this'll do for now.
instance Binary Object where
    get = liftM2 Object get get
    put (Object info refs) = put info >> put refs

instance Binary Ref where
    get = liftM2 Ref get get
    put (Ref hash info) = put hash >> put info

instance Binary Info where
    get = liftM2 Info get get
    put (Info label datamap) = put label >> put datamap