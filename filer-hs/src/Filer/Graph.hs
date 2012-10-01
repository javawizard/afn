
module Filer.Graph where

import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B
import Data.Binary (Binary(put, get), Put, Get, decode, encode)
import Filer.Hash (Hash, toHex)
import qualified Filer.FileUtils as F
import Control.Monad (liftM2)


type DataMap = M.Map String B.ByteString

data DB = DB FilePath

-- An Info has a type, which is a string, and a map of additional attributes
-- present on the info. Every object has an info and every ref has an info.
-- For example, folders are stored as an object with an info whose type is
-- "folder", and which has a number of refs, each one's type being "child" and
-- with a single attribute named "name" whose value is the name of the file
-- under which the referred commit is to be stored.
data Info = Info String DataMap
    deriving (Eq, Ord, Read, Show)

-- A ref is a pointer to an object, along with a type and some associated
-- attributes. Every object has zero or more refs.
data Ref = Ref Hash Info
    deriving (Eq, Ord, Read, Show)

-- An object is an entry in the graph database. It is uniquely identified by
-- its SHA-256 hash, and it contains a type, zero or more attributes, and zero
-- or more refs.
data Object = Object Info [Ref]
    deriving (Eq, Ord, Read, Show)

-- We're using instances of Binary to encode/decode things. This may or may
-- not be a good idea as I'm not sure how encoding plays with older versions of
-- Haskell, and I'd like to be able to separately define the encoding for 
-- objects so that things written in langauges other than Haskell can read and
-- write databases. But this'll do for now.
instance Binary Object where
    get = liftM2 Object get get
    put (Object info refs) = put info >> put refs

instance Binary Ref where
    get = liftM2 Ref get get
    put (Ref hash info) = put hash >> put info

instance Binary Info where
    get = liftM2 Info get get
    put (Info label datamap) = put label >> put datamap



readObject :: DB -> Hash -> IO Object
readObject db hash = decodeFile $ getObjectPath db hash

getObjectPath :: DB -> Hash -> FilePath
getObjectPath (DB dbPath) hash = dbPath `F.child` "objects" `F.child` (toHex hash)

















