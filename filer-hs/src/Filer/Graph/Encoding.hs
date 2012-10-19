
module Encoding where
import Data.Binary (Binary, get, put, encode, decode)
import Data.Set (Set)
import Filer.Hash (Hash, makeHash)

data Value
    = IntValue Integer
    | StringValue String
    | BinaryValue B.ByteString
    | BoolValue Bool

type DataMap = M.Map Value


hashObject :: (DataMap, Set (Hash, DataMap)) -> Hash
hashObject object = makeHash $ encode object














