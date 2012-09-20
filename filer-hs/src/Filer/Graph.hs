
module Filer.Graph where

import qualified Data.Map as M
import qualified Data.ByteString as B
import Filer.Hash (Hash)


type DataMap = Map String B.ByteString

data DB = DB FilePath

data Ref = Ref String Hash DataMap

data Object = Object String DataMap [Ref]


readObject :: DB -> Hash -> 

