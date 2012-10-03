
module Filer.Utils where

import Data.Word
import Data.ByteString
import Data.ByteString.Lazy


translateEnum :: (Enum a, Enum b) => a -> b
translateEnum = toEnum . fromEnum

translateList :: (Enum a, Enum b) => [a] -> [b]
translateList = map translateEnum

strictToLazy :: Data.ByteString.ByteString -> Data.ByteString.Lazy.ByteString
strictToLazy = Data.ByteString.Lazy.fromChunks . (:[])

lazyToStrict :: Data.ByteString.Lazy.ByteString -> Data.ByteString.ByteString
lazyToStrict = Data.ByteString.concat . Data.ByteString.Lazy.toChunks


