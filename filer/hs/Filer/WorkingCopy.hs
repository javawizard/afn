
module WorkingCopy where
import qualified Filer.FileUtils as F
import qualified Filer.Constants as C
import Control.Monad (forM_)


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
                
