
module Filer.Repository where

import Data.ByteString as B
import Filer.Utils (bytesToString, stringToBytes)


-- FilePath is the path to the .filer folder
data Repository = Repository FilePath

data ObjectType = Blob | Tree | Commit | Changeset


getRepoPath (Repository path) = path





