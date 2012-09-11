
module Filer.Repository where

import qualified Data.ByteString as B
import Filer.Utils (translateList)


-- FilePath is the path to the .filer folder
data Repository = Repository FilePath

data ObjectType = Blob | Tree | Commit | Changeset


getRepoPath (Repository path) = path





