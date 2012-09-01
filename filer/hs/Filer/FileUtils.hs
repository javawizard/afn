
module Filer.FileUtils where

import System.IO (FilePath)
import qualified System.XAttr as X
import System.IO.Error (catch)
import Data.ByteString
import Data.Maybe (isJust)


xattrGet :: FilePath -> String -> IO (Maybe String)
xattrGet file name = catch (liftM Just $ X.getxattr file name) (\_ -> return Nothing)

xattrHas :: FilePath -> String -> IO Bool
xattrHas file name = liftM isJust $ xattrGet file name

xattrSet :: FilePath -> String -> String -> IO ()
xattrSet = X.setXAttr

xattrDelete :: FilePath -> String -> IO ()
xattrDelete = x.removeXAttr

xattrList :: FilePath -> IO [String]
xattrList = x.listXAttr

isFolder :: FilePath -> IO Bool

