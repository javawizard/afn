
module Filer.Repository where


-- FilePath is the path to the .filer folder
data Repository = Repository FilePath


getRepoPath (Repository path) = path





