
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

type ChangesetMap = M.Map String String


getRepoPath (Repository path) = path

makeMagic :: String -> [Word8]
makeMagic = unpack . fromString

magicBlob = makeMagic "filerblb"
magicTree = makeMagic "filertre"
magicCommit = makeMagic "filercmt"
magicChangeset = makeMagic "filercst"

magicLength = 8

hReadObjectHeader :: Handle -> IO ObjectHeader
hReadObjectHeader handle = do
    headerData = liftM unpack $ hGet handle magicLength
    case headerData of
        magicBlob      -> return $ ObjectHeader Blob
        magicTree      -> return $ ObjectHeader Tree
        magicCommit    -> return $ ObjectHeader Commit
        magicChangeset -> return $ ObjectHeader Changeset
        _              -> error ("Invalid object header: " ++ headerData)  

hWriteObjectHeader :: Handle -> ObjectHeader -> IO ()
hWriteObjectHeader handle header = hPut handle $ pack $ case header of
    (ObjectHeader Blob)      -> magicBlob
    (ObjectHeader Tree)      -> magicTree
    (ObjectHeader Commit)    -> magicCommit
    (ObjectHeader Changeset) -> magicChangeset

createBlobFromFile :: Repository -> FilePath -> IO Hash

createTree :: Repository -> TreeMap -> IO Hash

createCommit :: Repository -> Commit -> IO Hash

createChangeset :: Repository -> ChangesetMap -> IO Hash

readBlobToHandle :: Repository -> Hash -> Handle -> IO ()

readBlobToFile :: Repository -> Hash -> FilePath -> IO ()

readTree :: Repository -> Hash -> IO TreeMap

readCommit :: Repository -> Hash -> IO Commit

readChangeset :: Repository -> Hash -> IO ChangesetMap

readObjectHeader :: Repository -> Hash -> IO ObjectHeader
readObjectHeader repository hash = withFile (getObjectFile repository hash) hReadObjectHeader

readObjectType :: Repository -> Hash -> IO ObjectType
readObjectType repository hash = do
    header <- readObjectHeader repository hash
    case header of (ObjectHeader type) -> return type

getObjectFile :: Repository -> Hash -> FilePath








































