
module Filer.Hash where

import Data.ByteString.UTF8 (fromString, toString)


data Hash = Hash [Word8]

wordToHex :: Word -> String
wordToHex = printf "%02x\n"

wordsToHex :: [Word] -> String
wordsToHex d = concat $ map wordToHex d

toHex :: Hash -> String
toHex (Hash words) = wordsToHex words

fromHex :: String -> Hash
fromHex d = fromMaybe (error "Not a valid hex hash") (maybeFromHex d)

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








