{-# LANGUAGE RecordWildCards #-}

module Zelden.IRC where

import Zelden.Protocol
import Control.Concurrent.STM
import Control.Concurrent.STM.Utils
import Control.Concurrent.STM.SaneTChan
import qualified Network.IRC.Base as I
import qualified Data.Map as M
import Zelden.Delay
import Control.Monad.Trans.Cont
import Zelden.Utils
import Control.Monad.Trans.Either
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Network.IRC.Base
import Control.Monad
import Data.Maybe
import Control.Monad.IO.Class
import Network.IRC.Parser
import Control.Var
import Data.List.Split
import Control.Monad.Trans
import System.Timeout
import Network
-- import Network.Socket
import Zelden.IO
import Control.Concurrent


data IRCConnection2
    = IRCConnection2 {
        inEndpoint :: Endpoint (Maybe Message),
        outQueue :: Queue (Maybe Message),
        -- Nothing until we get a nick from the server
        cNick :: Maybe String,
        nicksToTry :: [String]
    }

instance Show IRCConnection2 where
    show (IRCConnection2 {..}) = "<IRCConnection2 nick: " ++ show cNick ++ ">"

data Thing = Timeout | M Message | A Action | D
    deriving (Show)

run3' :: (Event -> IO ()) -> IO (Action -> IO ())
run3' handler = do
    q <- atomically $ newQueue
    e <- atomically $ newEndpoint q
    forkIO $ runContT (run3 e handler) return
    return $ \a -> atomically $ writeQueue q a

run3 :: Endpoint Action -> (Event -> IO ()) -> ContT () IO ()
run3 actionEndpoint handleEvent = do
    connVar <- liftIO $ atomically $ newTVar (Nothing :: Maybe IRCConnection2)
    enabledVar <- liftIO $ atomically $ newTVar False
    -- A var that holds a var that holds a boolean. The inner var is created
    -- by registerDelay (which sets it to True after a certain delay), or
    -- manually created with newTVar False to cause the delay to expire
    -- immedately. The outer var, then, holds the current inner var we're
    -- dealing with; it gets set to a new inner var every time we call
    -- registerDelay or manually set it to newTVar False.
    connectTimeoutVar <- liftIO $ atomically $ newTVar True >>= newTVar
    loop $ \continueOuter breakOuter -> do
        enabled <- liftIO $ atomically $ readTVar enabledVar
        conn <- liftIO $ atomically $ readTVar connVar
        nextThing <- liftIO $ atomically $ do
            timedOutVar <- readTVar connectTimeoutVar
            timedOut <- readTVar timedOutVar
            cm <- readTVar connVar
            if timedOut
                then do
                    return Timeout
                else liftM A (readEndpoint actionEndpoint) `orElse` do
                    maybeNextMessage <- maybe retry (readEndpoint . inEndpoint) cm
                    case maybeNextMessage of
                        Nothing -> return D
                        Just m -> return $ M m
        -- See if we timed out, and if so, schedule the delay again.
        case nextThing of
            Timeout -> do
                delayVar <- liftIO $ registerDelay 20000000 -- 20 seconds. TODO: Make this configurable
                liftIO $ atomically $ writeTVar connectTimeoutVar delayVar
            _ -> return ()
        -- Then process this event
        case (nextThing, conn) of
            (Timeout, Nothing) -> do
                liftIO $ putStrLn "IRC: Trying to connect"
                -- FIXME: Need to figure out how we're going to pass the server
                -- to connect into the protocol. Hard-coded to
                -- irc.opengroove.org to at least test things out for now. Also
                -- might want to make the timeout configurable, or even better,
                -- figure out how to connect in a separate thread so that we
                -- don't block up processing of actions just because we're
                -- trying to connect.
                maybeSocket <- liftIO $ flip catch (const $ return Nothing) $ timeout 5000000 $ connectTo "irc.opengroove.org" $ PortNumber 6667
                case maybeSocket of
                    -- Timed out or didn't connect. Don't do anything, just
                    -- wait until the next timeout happens and it's time to
                    -- connect once again
                    Nothing -> return ()
                    Just s -> do
                        -- Connected. Create a set of queues for the socket
                        (inEndpoint, outQueue) <- liftIO $ streamSocket' s (decode . (++ "\n")) (Just . encode)
                        -- Create a connection. Stick an infinite list ["zelden1","zelden2",...] as nicksToTry.
                        liftIO $ atomically $ writeTVar connVar $ Just $ IRCConnection2 inEndpoint outQueue Nothing $ map (("zelden" ++) . show) $ iterate (+ 1) 1
                        -- Then write the initial user and nick messages.
                        liftIO $ atomically $ do
                            writeQueue outQueue $ Just $ Message Nothing "USER" $ replicate 4 "zelden"
                            writeQueue outQueue $ Just $ Message Nothing "NICK" ["zelden"]
            (Timeout, Just _) -> do
                -- Nothing to do right now. In the future, we should
                -- probably send things like WHO and ISON when we time out
                -- while connected.
                return ()
            (M (Message _ "433" _), Just c@IRCConnection2 {outQueue=q, nicksToTry=nextNick:remainingNicks, cNick=Nothing}) -> do
                -- Got a 433 and we don't yet have a nick, which means our
                -- initial nick was rejected. Try the next one in the list.
                liftIO $ putStrLn $ "IRC: Nick failed. Trying " ++ nextNick
                liftIO $ atomically $ writeQueue q $ Just $ Message Nothing "NICK" [nextNick]
                liftIO $ atomically $ writeTVar connVar $ Just $ c {nicksToTry=remainingNicks}
            (A (Action _ (JoinRoom room)), Just IRCConnection2 {outQueue=q}) -> do
                liftIO $ atomically $ writeQueue q $ Just $ Message Nothing "JOIN" [room]
            (A (Action _ (PartRoom room reason)), Just IRCConnection2 {outQueue=q}) -> do
                liftIO $ atomically $ writeQueue q $ Just $ Message Nothing "PART" [room, reason]
            (A (Action _ (SendUserMessage user message)), Just IRCConnection2 {outQueue=q}) -> do
                liftIO $ atomically $ writeQueue q $ Just $ Message Nothing "PRIVMSG" [user, message]
            (A (Action _ (SendRoomMessage room message)), Just IRCConnection2 {outQueue=q}) -> do
                liftIO $ atomically $ writeQueue q $ Just $ Message Nothing "PRIVMSG" [room, message]
            (A (Action _ (SwitchSelfKey key)), Just IRCConnection2 {outQueue=q}) -> do
                liftIO $ atomically $ writeQueue q $ Just $ Message Nothing "NICK" [key]
            (A (Action _ (SetRoomTopic room topic)), Just IRCConnection2 {outQueue=q}) -> do
                liftIO $ atomically $ writeQueue q $ Just $ Message Nothing "TOPIC" [room, topic]
            (A (Action _ Shutdown), c) -> do
                case c of
                    Nothing -> return ()
                    Just IRCConnection2 {outQueue=outQueue, cNick=maybeCurrentNick} -> do
                        liftIO $ atomically $ writeQueue outQueue Nothing
                        when (isJust maybeCurrentNick) $ liftIO $ handleEvent $ Event M.empty Disconnected
                breakOuter ()
            (M (Message _ "PING" [pingData]), Just IRCConnection2 {outQueue=q}) -> do
                liftIO $ atomically $ writeQueue q $ Just $ Message Nothing "PONG" [pingData]
            (M (Message _ "001" (nick:values)), Just (c@IRCConnection2 {cNick=Nothing})) -> do
                -- Initial message, so notify Connected and set our nick
                liftIO $ atomically $ writeTVar connVar $ Just $ c {cNick=Just nick}
                liftIO $ handleEvent $ Event M.empty $ Connected nick
            (M (Message (Just (NickName oldNick _ _)) "NICK" [newNick]), Just (c@IRCConnection2 {cNick=Just currentNick}))
                | oldNick == currentNick -> do
                    -- Self rename
                    liftIO $ atomically $ writeTVar connVar $ Just $ c {cNick=Just newNick}
                    liftIO $ handleEvent $ Event M.empty $ UserSwitchedKey oldNick newNick
                | otherwise -> do
                    -- Someone else renamed
                    liftIO $ handleEvent $ Event M.empty $ UserSwitchedKey oldNick newNick
            (M (Message (Just (NickName fromNick _ _)) "JOIN" [room]), Just c) -> do
                -- Us or someone else joined
                liftIO $ handleEvent $ Event M.empty $ UserJoinedRoom room fromNick
            (M (Message _ "353" [_, _, channel, userString]), Just c) -> do
                -- TODO: Somehow track whether we've received this for a
                -- channel yet to prevent issuing duplicate joins if we ever
                -- issue a NAMES manually.
                forM (splitOn " " userString) $ \user -> do
                    -- Ignore the mode for now, if one's present. I'm thinking
                    -- in the future I'll break tradition a bit and issue
                    -- modes as a separate event, as if the user had been
                    -- granted the mode just after we see them join the
                    -- channel. Although I might not end up doing that, I'm not
                    -- sure yet.
                    let nick = case user of
                        -- TODO: Change this to use the server's 005 PREFIX
                        prefix:nick | prefix `elem` "@+%&~" -> nick
                        nick                                -> nick
                    liftIO $ handleEvent $ Event M.empty $ UserJoinedRoom room nick
            (M (Message (Just (NickName fromNick _ _)) "PART" (room:maybeReason)), Just c) -> do
                -- Us or someone else parted a room
                liftIO $ handleEvent $ Event M.empty $ UserPartedRoom room fromNick $ Parted $ fromMaybe "" $ listToMaybe maybeReason
            (M (Message (Just (NickName fromNick _ _)) "QUIT" maybeReason), Just c) -> do
                -- Us or someone else quit
                liftIO $ handleEvent $ Event M.empty $ UserQuit fromNick $ fromMaybe "" $ listToMaybe maybeReason
            (M (Message (Just (NickName fromNick _ _)) "PRIVMSG" [recipient, message]), Just (c@IRCConnection2 {cNick=Just currentNick}))
                | recipient == currentNick -> do
                    -- Direct message to us
                    liftIO $ handleEvent $ Event M.empty $ UserMessage fromNick message
                | otherwise -> do
                    -- Message to a room. TODO: Might need to double-check here
                    -- that it's actually a valid room, as I seem to recall
                    -- that some servers will deliver messages with weird
                    -- recipients when snooping in on direct messages when an
                    -- op...
                    liftIO $ handleEvent $ Event M.empty $ RoomMessage recipient fromNick message
            (D, Just (c@IRCConnection2 {cNick=maybeCurrentNick})) -> do
                -- Disconnected. If we have a nick (which means we've sent Connected),
                -- send Disconnected. Then nix out the connection. If we don't
                -- actually have a connection, then we shouldn't be getting
                -- this in the first place. We'll also clear the timeout to
                -- cause an immediate attempt to reconnect if we're enabled.
                when (isJust maybeCurrentNick) $ liftIO $ handleEvent $ Event M.empty Disconnected
                liftIO $ atomically $ writeTVar connVar Nothing >> newTVar False >>= writeTVar connectTimeoutVar
                
            (t, cm) -> liftIO $ putStrLn $ "IRC: Unhandled thing from connection " ++ show cm ++ ": " ++ show t
        continueOuter
    liftIO $ putStrLn "IRC loop exiting."


waitForTrue :: a -> TVar Bool -> STM a
waitForTrue constant var = do
    value <- readTVar var
    if value
        then return constant
        else retry









































