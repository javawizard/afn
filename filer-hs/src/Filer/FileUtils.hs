
module Filer.FileUtils where

import System.IO (FilePath)
import qualified System.XAttr as X
import System.IO.Error (catch)
import Data.ByteString
import Data.Maybe (isJust)
import Control.Monad (liftM, liftM2)
import qualified System.FilePath as F
import qualified System.Directory as D 

-- Gets an xattr, returning Nothing if it doesn't exist.
xattrGet :: FilePath -> String -> IO (Maybe String)
xattrGet file name = catch (liftM Just $ X.getXAttr file name) (\_ -> return Nothing)

-- True if the specified xattr exists, false if it doesn't.
xattrHas :: FilePath -> String -> IO Bool
xattrHas file name = liftM isJust $ xattrGet file name

-- Sets the specified name to the specified value.
xattrSet :: FilePath -> String -> String -> IO ()
xattrSet = X.setXAttr

-- Deletes the specified xattr.
xattrDelete :: FilePath -> String -> IO ()
xattrDelete = X.removeXAttr

-- Lists the names of all xattrs.
xattrList :: FilePath -> IO [String]
xattrList = X.listXAttr

-- True if it's a folder, false if it isn't.
isFolder :: FilePath -> IO Bool
isFolder = D.doesDirectoryExist

-- True if it exists, false if it doesn't.
exists :: FilePath -> IO Bool
exists path = liftM2 (||) (D.doesFileExist path) (D.doesDirectoryExist path) 

-- The children of this folder.
children :: FilePath -> IO [FilePath]
children = D.getDirectoryContents

-- Deletes this file or folder; folders must not have any contents at present.
delete :: FilePath -> IO ()
delete file = do
    f <- isFolder file
    if f
        then D.removeDirectory file
        else D.removeFile file

-- Creates the specified folder
mkdir :: FilePath -> IO ()
mkdir = D.createDirectory

child :: FilePath -> String -> FilePath
child = F.combine



