
module Filer.WorkingCopy where
import qualified Filer.FileUtils as F
import qualified Filer.Constants as C
import qualified Filer.Repository as R
import qualified Filer.Hash as H
import Data.List.Split (splitOn)
import Control.Monad (forM_)

-- A working copy; the first argument is the repository and the second argument
-- is the working file/folder. Note that this file will usually have a
-- C.xattrRepo xattr, but it is not required to (e.g. when exporting a
-- revision; this is done by creating a working copy without a C.xattrRepo).
data WorkingCopy = WorkingCopy Repository FilePath

split :: String -> [String]
split a = case splitOn " " a of
    [""] -> []
    xs   -> xs

maybeReadBase :: FilePath -> IO (Maybe [Hash])
maybeReadBase path = do
    maybeXattr = F.xattrGet path C.xattrBase
    case maybeXattr of
        Nothing    -> return Nothing
        Just xattr -> return $ map H.fromHex $ split xattr

readBase :: FilePath -> IO [Hash]
readBase path = do
    maybeBase <- maybeReadBase path
    case maybeBase of
        Nothing   -> error $ path ++ " doesn't have the xattr " ++ C.xattrBase
        Just base -> return base

writeBase :: FilePath -> [Hash] -> IO ()
writeBase path hashes = F.xattrSet path C.xattrBase $ intercalate " " $ map H.toHex hashes

-- Deletes if tracked, unless it's a dir that has untracked children, or it has
-- a C.xattrRepo; these are simply untracked.
untrackAndDelete :: FilePath -> IO ()
untrackAndDelete file = do
    isTracked <- F.xattrHas file C.xattrBase
    -- Untrack it if it's tracked
    when isTracked $ F.xattrDelete file C.xattrBase
    -- If it's a folder, recursively untrackAndDelete its contents
    when F.isFolder file $ do
        children <- F.children file
        forM_ children untrackAndDelete
    -- If it does not have C.xattrRepo, and if it's either a file or a folder
    -- with no contents, delete it.
    hasRepo <- F.xattrHas file C.xattrRepo
    unless hasRepo $ do
        isFolder <- C.isFolder file
        hasContents <- do
            if isFolder
                then do
                    contents <- F.children file
                    return $ contents == []
                else
                    return False
        unless hasContents $ F.delete file

getWorkingPath :: WorkingCopy -> FilePath
getWorkingPath (WorkingCopy _ workingPath) = workingPath

getRepository :: WorkingCopy -> Repository
getRepository (WorkingCopy repository _) = repository

createWorkingCopy :: WorkingCopy -> IO ()
createWorkingCopy workingCopy = do
    return ()

-- Updates the specified working copy to the specified commit.
updateWorkingCopy :: WorkingCopy -> Hash -> IO ()
updateWorkingCopy (WorkingCopy repository workingPath) hash = do
    -- Delete the file/folder so that we start off with a clean slate. We
    -- really should change this later to avoid having to reconstruct
    -- everything.
    untrackAndDelete workingPath
    -- Now we read the commit
    commit <- R.readCommit repository hash
    let (Commit changesetHash objectHash parents) = commit
    -- And then we read the object's type
    objectType <- R.readObjectType repository objectHash
    case objectType of
        R.Tree -> do
            tree <- R.readTree repository objectHash
            isFolder <- R.isFolder workingPath
            unless isFolder $ F.mkdir workingPath
            
        R.Blob -> do
            return ()
        _      -> error "Commit points to a non-tree/blob object"














