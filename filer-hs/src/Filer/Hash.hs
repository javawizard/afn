
module Filer.Hash where

data Hash = Hash [Word8]

toHex :: Hash -> String

fromHex :: String -> Hash

maybeFromHex :: String -> Maybe Hash

toBinary :: Hash -> [Word8]
toBinary (Hash words) = words

fromBinary :: [Word8] -> Hash
fromBinary d = fromMaybe (error "Not a valid binary hash") (maybeFromBinary d) 

maybeFromBinary -> [Word8] -> Maybe Hash
maybeFromBinary d = if length d == hashBinaryLength
    then Just $ Hash d
    else Nothing
    
hashBinaryLength = 32

hashHexLength = 64

readBinaryHash :: Handle -> IO Hash

readHexHash :: Handle -> IO Hash








