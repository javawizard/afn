
module Filer.Utils where

import Data.Word


translate :: (Enum a, Enum b) => a -> b
translate = toEnum . fromEnum

translateList :: (Enum a, Enum b) => [a] -> [b]
translateList = map . translate

bytesToString :: [Word8] -> String
bytesToString = translateList

stringToBytes :: String -> [Word8]
stringToBytes = translateList


