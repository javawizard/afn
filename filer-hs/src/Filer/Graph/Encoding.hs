
module Filer.Graph.Encoding where
import Data.Binary (Binary, get, put, Get, Put, encode, decode)
import Data.Set (Set)
import Filer.Hash (Hash, makeHash)
import Data.ByteString.Lazy (ByteString)
import Data.Word (Word8)
import Control.Monad (liftM)
import qualified Data.Map as M

data Value
    = IntValue Integer
    | StringValue String
    | BinaryValue ByteString
    | BoolValue Bool
    deriving (Eq, Ord, Read, Show)

instance Binary Value where
    get = do
        tag <- get :: Get Word8
        case tag of
            1 -> liftM IntValue get
            2 -> liftM StringValue get
            3 -> liftM BinaryValue get
            4 -> liftM BoolValue get
            _ -> error $ "Invalid value tag: " ++ (show tag)
    
    put (IntValue i) = put (1 :: Word8) >> put i
    put (StringValue s) = put (2 :: Word8) >> put s
    put (BinaryValue b) = put (3 :: Word8) >> put b
    put (BoolValue b) = put (4 :: Word8) >> put b 

type DataMap = M.Map String Value


hashObject :: DataMap -> Set (Hash, DataMap) -> Hash
hashObject attributes refs = makeHash $ encodeObject attributes refs

encodeObject :: DataMap -> Set (Hash, DataMap) -> ByteString
encodeObject attributes refs = encode (attributes, refs)

decodeObject :: ByteString -> (DataMap, Set (Hash, DataMap))
decodeObject bytes = decode bytes














