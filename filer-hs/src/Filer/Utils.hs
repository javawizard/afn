
module Filer.Utils where

import Data.Word


translateEnum :: (Enum a, Enum b) => a -> b
translateEnum = toEnum . fromEnum

translateList :: (Enum a, Enum b) => [a] -> [b]
translateList = map . translateEnum


