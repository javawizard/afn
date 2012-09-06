
module WorkingCopy where
import qualified Filer.FileUtils as F
import qualified Filer.Constants as C
import Control.Monad (forM_)

-- A working copy; the first argument is the repository and the second argument
-- is the working file/folder. Note that this file will usually have a
-- C.xattrRepo xattr, but it is not required to (e.g. when exporting a
-- revision; this is done by creating a working copy without a C.xattrRepo).
data WorkingCopy = WorkingCopy Repository FilePath


-- Deletes if tracked, unless it's a dir that has untracked children; these are
-- simply untracked.
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















