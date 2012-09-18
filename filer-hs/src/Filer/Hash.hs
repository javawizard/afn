
module Filer.Hash where

import Data.ByteString.UTF8 (fromString, toString)
import Data.ByteString (pack, unpack, hGet)
import qualified Data.Hex as Hex


data Hash = Hash [Word8]

hexToBinary :: String -> Maybe [Word8]
hexToBinary d = do
    -- Unhex it, passing along Nothing if unhex can't understand it
    b <- unhex $ fromString $ d
    return $ unpack b

binaryToHex :: [Word8] -> String
binaryToHex d = toString $ hex $ pack $ d

toHex :: Hash -> String
toHex (Hash words) = binaryToHex words

fromHex :: String -> Hash
fromHex d = fromMaybe (error "Not a valid hex hash") (maybeFromHex d)

maybeFromHex :: String -> Maybe Hash
maybeFromHex text = do
    b <- hexToBinary text
    maybeFromBinary b

toBinary :: Hash -> [Word8]
toBinary (Hash words) = words

fromBinary :: [Word8] -> Hash
fromBinary d = fromMaybe (error "Not a valid binary hash") (maybeFromBinary d) 

maybeFromBinary -> [Word8] -> Maybe Hash
maybeFromBinary d = if length d == hashBinaryLength
    then Just $ Hash d
    else Nothing
    
hashBinaryLength = 32

hashHexLength = hashBinaryLength * 2

readBinaryHash :: Handle -> IO Hash
readBinaryHash handle = do
    bytes <- liftM unpack $ hGet handle hashBinaryLength
    return $ fromBinary bytes

readHexHash :: Handle -> IO Hash
readHexHash handle = do
    text <- liftM toString $ hGet handle hashHexLength
    return $ fromHex text








