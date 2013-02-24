
module Zelden.IO where

import System.IO
import Control.Monad (liftM, when)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.SaneTChan
import Network
import Network.Socket
import Data.Conduit
-- import Network.BSD

streamSocket :: Socket -> (String -> Maybe i) -> (o -> Maybe String) -> Queue (Maybe i) -> Endpoint (Maybe o) -> IO ()
streamSocket socket inputConverter outputConverter inputQueue outputEndpoint = do
    lineSocket <- newLineSocket socket
    -- Start input thread
    forkIO $ do
        let doneWithSocket = do
            -- Send Nothing to the input queue
            atomically $ writeQueue inputQueue Nothing
            -- Close the handle, just in case
            catch (sClose socket) $ const $ return ()
            return Nothing
        let process = do
            -- Read a line from the socket, calling doneWithSocket on errors
            line <- catch (liftM Just $ getLineFrom lineSocket) $ const doneWithSocket
            case line of
                Nothing -> return ()
                Just l -> do
                    case inputConverter l of
                        Nothing -> return ()
                        Just thing -> (atomically $ writeQueue inputQueue $ Just thing)
                    process
        process
    -- Start output thread
    forkIO $ do
        let process = do
            -- Read the next item from the endpoint
            nextItem <- atomically $ readEndpoint outputEndpoint
            case nextItem of
                -- If it's Nothing, close the socket
                Nothing -> (catch (sClose socket) $ const $ return ())
                -- Otherwise, write the (converted) message out and read another
                -- item from the endpoint
                Just m -> do
                    case outputConverter m of
                        Nothing -> return ()
                        Just thing -> sendAll socket (thing ++ "\n")
                    process
        process
    return ()

streamSocket' :: Socket -> (String -> Maybe i) -> (o -> Maybe String) -> IO (Endpoint (Maybe i), Queue (Maybe o))
streamSocket' handle inputConverter outputConverter = do
    (iq, ie, oq, oe) <- atomically $ do
        iq <- newQueue
        ie <- newEndpoint iq
        oq <- newQueue
        oe <- newEndpoint oq
        return (iq, ie, oq, oe)
    streamSocket handle inputConverter outputConverter iq oe
    return (ie, oq)

data LineSocket = LineSocket Socket (TVar String)

newLineSocket :: Socket -> IO LineSocket
newLineSocket s = do
    var <- atomically $ newTVar ""
    return $ LineSocket s var

getLineFrom :: LineSocket -> IO String
getLineFrom lineSocket@(LineSocket socket var) = do
    value <- atomically $ readTVar var
    if '\n' `elem` value
        then do
            let (result, _:rest) = break (== '\n') value
            atomically $ writeTVar var rest
            return result
        else do
            socketData <- recv socket 1024
            when (socketData == "") $ error "End of input"
            atomically $ writeTVar var $ value ++ socketData
            getLineFrom lineSocket

sendAll :: Socket -> String -> IO ()
sendAll _ "" = return ()
sendAll socket socketData = do
    amount <- send socket socketData
    sendAll socket $ drop amount socketData



linesP :: MonadIO m => Conduit String String m ()
linesP = linesP' ""

linesP' :: MonadIO m => String -> Conduit String String m ()
linesP' leftover = do
    if '\n' `elem` leftover
        then do
            let (result, _:rest) = break (== '\n') value
            -- TODO: Might want to check for (and strip) a trailing \r
            yield result
            linesP' rest
        else do
            nextData <- await
            -- TODO: Probably ought to yield the leftovers we were passed as
            -- the last line, in case the connection was closed without a
            -- trailing newline
            if (nextData == "")
                then return ()
                else linesP' $ leftover ++ nextData

writeToQueueP :: MonadIO m => Queue a -> Conduit a () m ()
writeToQueueP q -> do
    value <- await
    liftIO $ atomically $ writeQueue q a
    writeToQueueP q







