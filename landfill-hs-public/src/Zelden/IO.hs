
module Zelden.IO where

import System.IO
import Control.Monad (liftM)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.SaneTChan
import Network

streamSocket :: Handle -> (String -> Maybe i) -> (o -> Maybe String) -> Queue (Maybe i) -> Endpoint (Maybe o) -> IO ()
streamSocket handle inputConverter outputConverter inputQueue outputEndpoint = do
    -- Start input thread
    forkIO $ do
        let doneWithSocket = do
            putStrLn "1"
            -- Send Nothing to the input queue
            atomically $ writeQueue inputQueue Nothing
            putStrLn "2"
            -- Close the handle, just in case
            hClose handle
            putStrLn "3"
            return Nothing
        let process = do
            -- Read a line from the socket, calling doneWithSocket on errors
            putStrLn "4"
            line <- catch (liftM Just $ hGetLine handle) $ const doneWithSocket
            putStrLn "5"
            case line of
                Nothing -> putStrLn "6" >> return ()
                Just l -> do
                    putStrLn "7"
                    case inputConverter l of
                        Nothing -> putStrLn "8" >> return ()
                        Just thing -> putStrLn "9" >> (atomically $ writeQueue inputQueue $ Just thing)
                    putStrLn "10" 
                    process
        putStrLn "11"
        process
    -- Start output thread
    forkIO $ do
        putStrLn "12"
        let process = do
            putStrLn "13"
            -- Read the next item from the endpoint
            nextItem <- atomically $ readEndpoint outputEndpoint
            putStrLn "14"
            case nextItem of
                -- If it's Nothing, close the socket
                Nothing -> putStrLn "15" >> hClose handle
                -- Otherwise, write the (converted) message out and read another
                -- item from the endpoint
                Just m -> do
                    putStrLn "16"
                    case outputConverter m of
                        Nothing -> putStrLn "17" >> return ()
                        Just thing -> putStrLn "18" >> hPutStrLn handle thing
                    putStrLn "19" 
                    process
        putStrLn "20"
        process
    putStrLn "21"
    return ()

streamSocket' :: Handle -> (String -> Maybe i) -> (o -> Maybe String) -> IO (Endpoint (Maybe i), Queue (Maybe o))
streamSocket' handle inputConverter outputConverter = do
    (iq, ie, oq, oe) <- atomically $ do
        iq <- newQueue
        ie <- newEndpoint iq
        oq <- newQueue
        oe <- newEndpoint oq
        return (iq, ie, oq, oe)
    streamSocket handle inputConverter outputConverter iq oe
    return (ie, oq)

