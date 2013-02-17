
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
            -- Send Nothing to the input queue
            atomically $ writeQueue inputQueue Nothing
            -- Close the handle, just in case
            hClose handle
            return Nothing
        let process = do
            -- Read a line from the socket, calling doneWithSocket on errors
            line <- catch (liftM Just $ hGetLine handle) $ const doneWithSocket
            case line of
                Nothing -> return ()
                Just l -> do
                    case inputConverter l of
                        Nothing -> return ()
                        Just thing -> atomically $ writeQueue inputQueue $ Just thing 
                    process
        process
    -- Start output thread
    forkIO $ do
        let process = do
            -- Read the next item from the endpoint
            nextItem <- atomically $ readEndpoint outputEndpoint
            case nextItem of
                -- If it's Nothing, close the socket
                Nothing -> hClose handle
                -- Otherwise, write the (converted) message out and read another
                -- item from the endpoint
                Just m -> do
                    case outputConverter m of
                        Nothing -> return ()
                        Just thing -> hPutStrLn handle thing 
                    process
        process
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

