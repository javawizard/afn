
module Encoding where
import Data.Binary (Binary, get, put, encode, decode)
import Data.Set (Set)
import Filer.Hash (Hash, makeHash)
import Data.ByteString.Lazy (ByteString)

data Value
    = IntValue Integer
    | StringValue String
    | BinaryValue ByteString
    | BoolValue Bool
    deriving (Eq, Ord, Read, Show)

type DataMap = M.Map String Value


hashObject :: DataMap -> Set (Hash, DataMap) -> Hash
hashObject attributes refs = makeHash $ encodeObject attributes refs

encodeObject :: DataMap -> Set (Hash, DataMap) -> ByteString
encodeObject attributes refs = encode (attributes, refs)

decodeObject :: ByteString -> (DataMap, Set (Hash, DataMap))
decodeObject bytes = decode bytes














