
module Filer.Graph where

import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Binary (Binary(put, get), Put, Get)
import Filer.Hash (Hash)


type DataMap = Map String B.ByteString

data DB = DB FilePath

-- An Info has a type, which is a string, and a map of additional attributes
-- present on the info. Every object has an info and every ref has an info.
-- For example, folders are stored as an object with an info whose type is
-- "folder", and which has a number of refs, each one's type being "child" and
-- with a single attribute named "name" whose value is the name of the file
-- under which the referred commit is to be stored.
data Info = Info String DataMap

-- A ref is a pointer to an object, along with a type and some associated
-- attributes. Every object has zero or more refs.
data Ref = Ref Hash Info

-- An object is an entry in the graph database. It is uniquely identified by
-- its SHA-256 hash, and it contains a type, zero or more attributes, and zero
-- or more refs.
data Object = Object Info [Ref]

instance Binary Object where
    get = do
        info <- get
        refs <- get
        return $ Object info refs
    
    put (Object info refs) = do
        put info
        put refs

instance Binary Ref where
    get = do
        hash <- get
        info <- get
        return $ Ref hash info
    
    put (Ref hash info) = do
        put hash
        put info

instance Binary Info where
    get = do
        label <- get
        datamap <- get
        return $ Info label datamap
    
    put (Info label datamap) = do
        put label
        put datamap

{-
So we need a data format...

-}


readObject :: DB -> Hash -> 

