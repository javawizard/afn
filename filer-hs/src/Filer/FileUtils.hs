
module Filer.FileUtils where

import System.IO (FilePath)
import qualified System.XAttr as X
import System.IO.Error (catch)
import Data.ByteString
import Data.Maybe (isJust)

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
xattrDelete = x.removeXAttr

-- Lists the names of all xattrs.
xattrList :: FilePath -> IO [String]
xattrList = x.listXAttr

-- True if it's a folder, false if it isn't.
isFolder :: FilePath -> IO Bool

-- True if it exists, false if it doesn't.
exists :: FilePath -> IO Bool

-- The children of this folder.
children :: FilePath -> IO [FilePath]

-- Deletes this file or folder; folders must not have any contents at present.
delete :: FilePath -> IO ()

-- Creates the specified folder
mkdir :: FilePath -> IO ()



