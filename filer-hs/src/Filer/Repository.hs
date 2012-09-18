
module Filer.Repository where

import qualified Data.ByteString as B
import Filer.Utils (translateList)


-- FilePath is the path to the .filer folder
data Repository = Repository FilePath

data ObjectType = Blob | Tree | Commit | Changeset

data ObjectHeader = ObjectHeader ObjectType


getRepoPath (Repository path) = path

makeMagic :: String -> [Word8]
makeMagic = unpack . fromString

magicBlob = makeMagic "filerblb"
magicTree = makeMagic "filertre"
magicCommit = makeMagic "filercmt"
magicChangeset = makeMagic "filercst"

magicLength = 8

readObjectHeader :: Handle -> IO ObjectHeader
readObjectHeader handle = do
    headerData = hGet handle magicLength
    case headerData of
        magicBlob      -> return $ ObjectHeader Blob
        magicTree      -> return $ ObjectHeader Tree
        magicCommit    -> return $ ObjectHeader Commit
        magicChangeset -> return $ ObjectHeader Changeset
        _              -> error ("Invalid object header: " ++ headerData)  





