
module SimpleHTTP where

import Network
import System.IO
import Data.List.Split (splitOn)
import Control.Monad

main = do
    server <- listenOn $ PortNumber 8000
    forever $ do
        (handle, host, port) <- accept server
        isEnd <- hIsEOF handle
        if isEnd
            then hClose handle
            else processConnection handle

processConnection handle = do
    openingLine <- hGetLine handle
    let [method, path, _] = splitOn " " openingLine
    putStrLn $ "Requested: " ++ path
    hClose handle
