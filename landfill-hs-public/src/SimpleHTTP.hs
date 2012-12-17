
module SimpleHTTP where

import Network
import System.IO
import Data.List.Split (splitOn)
import Control.Monad
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.List (splitAt, elemIndex)

main = do
    server <- listenOn $ PortNumber 8000
    forever $ do
        (handle, host, port) <- accept server
        isEnd <- hIsEOF handle
        if isEnd
            then hClose handle
            else processConnection handle

processConnection handle = do
    openingLine <- readLine handle
    let [method, pathAndQuery, httpVersion] = splitOn " " openingLine
    headerLines <- readUntilEmptyLine handle
    let headers = M.fromList (map (partition ':') headerLines)
    let (path, query) = partition '?' pathAndQuery
    putStrLn $ "Requested: " ++ path
    putStrLn $ "Query: " ++ query
    putStrLn $ "Headers: " ++ (show headers)
    let page = M.findWithDefault errorPage path pages
    page query handle
    hClose handle

readUntilEmptyLine :: Handle -> IO [String]
readUntilEmptyLine handle = do
    line <- readLine handle
    putStrLn $ "Line: " ++ show line
    case line of
        "" -> return []
        _  -> do
            remainingLines <- readUntilEmptyLine handle
            return $ line:remainingLines

partition :: (Eq a) => a -> [a] -> ([a], [a])
partition item values = splitAt (fromMaybe (length values) $ elemIndex item values) values

readLine :: Handle -> IO String
readLine handle = do
    line <- hGetLine handle
    if last line == '\r'
        then return $ init line
        else return line

sendLine :: Handle -> String -> IO ()
sendLine handle text = hPutStr handle (text ++ "\r\n")

sendResponse :: Handle -> Int -> String -> String -> IO ()
sendResponse handle code status contentType = do
    sendLine handle $ "HTTP/1.1 " ++ show code ++ " " ++ status
    sendLine handle "Server: SimpleHTTP.hs"
    sendLine handle $ "Content-Type: " ++ contentType
    sendLine handle ""






pages = M.fromList [
        ("/", homePage),
        ("/index.html", homePage),
        ("/sarah", sarahPage)
    ]

homePage query handle = do
    sendResponse handle 200 "OK" "text/html"
    sendLine handle "<html><body><b>Hi!</b> How are you?</body></html>"

sarahPage query handle = do
    sendResponse handle 200 "OK" "text/html"
    sendLine handle "<html><body>I love you Sarah :)</body></html>"

errorPage query handle = do
    putStrLn "error"
    sendResponse handle 404 "Not Found" "text/html"
    sendLine handle "<html><body>Sorry, that page wasn't found.</body></html>"









































