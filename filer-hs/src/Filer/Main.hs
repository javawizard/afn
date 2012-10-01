
module Filer.Main where

import qualified Filer.Constants
import qualified Filer.FileUtils
import qualified Filer.Graph
import qualified Filer.Hash
-- import qualified Filer.Repository
import qualified Filer.Utils
-- import qualified Filer.WorkingCopy

main :: IO ()
main = do
    putStrLn "Filer version control"
    putStrLn "http://hg.opengroove.org/afn"
    putStrLn "More to come soon."
