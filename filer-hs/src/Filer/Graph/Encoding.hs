
module Filer.Graph.Encoding where
import Data.Binary (Binary, get, put, Get, Put, encode, decode)
import Data.Set (Set)
import Filer.Hash (Hash, makeHash)
import Data.ByteString.Lazy (ByteString)
import Data.Word (Word8)
import Control.Monad (liftM)
import qualified Data.Map as M
import qualified Data.Set as S

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

data Pretty a b = a := b

prettyToTuple :: Pretty a b -> (a, b)
prettyToTuple (a := b) = (a, b)

tupleToPretty :: (a, b) -> Pretty a b
tupleToPretty (a, b) = a := b

type PrettyObject = Pretty [Pretty String Value] [Pretty String [Pretty String Value]]

toPretty :: DataMap -> S.Set (Hash, DataMap) -> PrettyObject
toPretty attrMap refSet = (map tupleToPretty $ M.toList attrMap) := (map (\(h, a) -> toHex h := (map tupleToPretty $ M.toList a)) $ S.toList refSet)

fromPretty :: PrettyObject -> (DataMap, S.Set (Hash, DataMap))
fromPretty (attrs := refs) = ((M.fromList $ map prettyToTuple attrs), S.fromList $ map (\(h, a) -> (fromHex h, M.fromList $ map prettyToTuple a)) refs)














