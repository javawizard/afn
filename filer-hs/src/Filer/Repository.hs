
module Filer.Repository where

import qualified Data.ByteString as B
import Filer.Utils (translateList)
import qualified Data.Map as M


-- FilePath is the path to the .filer folder
data Repository = Repository FilePath

data ObjectType = Blob | Tree | Commit | Changeset

data ObjectHeader = ObjectHeader ObjectType

-- The changeset hash, the blob/tree hash, and a list of parent hashes,
-- respectively
data Commit = Commit Hash Hash [Hash]

type TreeMap = M.Map String Hash


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
    headerData = liftM unpack $ hGet handle magicLength
    case headerData of
        magicBlob      -> return $ ObjectHeader Blob
        magicTree      -> return $ ObjectHeader Tree
        magicCommit    -> return $ ObjectHeader Commit
        magicChangeset -> return $ ObjectHeader Changeset
        _              -> error ("Invalid object header: " ++ headerData)  

writeObjectHeader :: Handle -> ObjectHeader -> IO ()
writeObjectHeader handle header = hPut handle $ pack $ case header of
    (ObjectHeader Blob)      -> magicBlob
    (ObjectHeader Tree)      -> magicTree
    (ObjectHeader Commit)    -> magicCommit
    (ObjectHeader Changeset) -> magicChangeset

createBlobFromFile :: Repository -> FilePath -> IO Hash

createTree :: Repository -> TreeMap -> IO Hash








































