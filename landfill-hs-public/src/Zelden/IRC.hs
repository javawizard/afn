
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

data IRCProtocol = IRCProtocol

data IRCConnection
    = IRCConnection {
        connConfigVars :: M.Map String String
        connEnabledVar :: TVar Boolean
        connSession :: TVar (Maybe IRCSession)
        connHandler :: EventCallback -- Event -> IO ()
    }

data IRCSession
    = IRCSession {
        sessionConnection :: IRCConnection,
        sessionReader :: Endpoint I.Message,
        sessionWriter :: Queue I.Message,
        sessionReadTimeout :: Maybe Int,
        sessionVars :: TVar (M.Map String String),
        sessionNick :: TVar String
    }

data DisconnectReason
    = SocketClosed
    | ReadTimedOut
    | TooManyNickTries
    

instance Protocol IRCProtocol where
    likesPastebin _ = True
    createConnection _ callback = do
        return IRCConnection

type IRCM a = ReaderT IRCSession (EitherT DisconnectReason IO)

{-
So, how are we going to actually do connections?

Well, let's have a dispatch thread that does all of the network communication.
(Oh, and I'm thinking the largeish bits of code that handle handshaking can be
in MaybeT or something, with a function in MaybeT to get the next value from
the queue representing the socket or abort with Nothing if the enabled/disabled
TVar gets set to disabled, which would automatically take care of disconnecting
us when we're requested to disconnect, even if in the middle of a handshake.
The same function could also use a timeout to abort with Nothing if the server
doesn't respond in a timely manner.)

So, the dispatch thread (using the function mentioned above) sits there and
retries until enabled is true. Then it makes one connection attempt to the
server, and once that's done and the server's disconnected, it sleeps for a
certain number of seconds (in the future, increasing depending on how many
times the connect attempt failed) and then goes back to the top, where it waits
for enabled to be true.

To make a connection attempt, the dispatch thread opens a socket (with a
timeout) to the server, creates queues and endpoints for it, then enters the
MaybeT IO block of code. That bit writes the USER message out, then writes
(incidentally, I'm thinking of having the read function also automatically
filter out PINGs and respond with PONGs accordingly, which will get us proper
PING/PONG support within handshakes, as some servers like to do) NICK messages
in a loop waiting for something other than the nick already in use status code.
Once it gets it, it stuffs it back onto the socket endpoint and continues.

Then we send a Connected event (TODO: are we allowed to issue Disconnected
multiple times? I'm tempted to say no, in which case we'll also need to store
into a TVar that we issued a Connected so that when we're done with the attempt
we need to issue a Disconnected) since once we stop getting nick already in use
back we know what our user key will be. Then we log all of the messages we
receive up to 376 (end of motd), when the handshake is done. We'll also store
off the 005 messages that arrive in some sort of capabilities dictionary, and
in particular we'll store off the PREFIX bit, which we'll use to map modes to
prefixes. (Most IRC client libraries I've seen aren't smart enough to work out
the server's prefixes, and instead rely on a hard-coded list; I really want
this IRC library to automatically detect what's what, and perhaps use a sane
default if the server never sends a 005 PREFIX.)

Then we go into normal message receive mode, where we deal with each message
that we receive as we receive it.

When we receive a privmsg, join channel, part channel, kick, quit, and so on,
we just issue the corresponding event.

When we receive a message that we don't recognize, we issue a server custom
event with information about what happened. (TODO: create such an event, and
figure out how to specify a user key that means a server, or just use our
actual server's name for now; need to think about how to do this. IRC doesn't
really care which server the message came from, but XMPP does, as a message
from opengroove.org is very different from a message from talk.google.com.)

Oh, when we finish the handshake, we also store off into a variable that we're
now connected. (Or maybe we just don't issue the Connected event until after
we've received the 376, and rely on Zelden not to send us stuff if we haven't
issued Connected yet. Although maybe that's a bad idea, as that would seem
prone to race conditions.)

Actually no, IRC servers seem perfectly content to ignore you (or rather, send
you an error response, I forget which code now) if you try to join channels or
send messages while in the middle of a handshake, so I suppose that doesn't
matter quite as much. Although might still be a good idea to filter things out
if we're in the middle of a handshake.

So yeah, right before we send Connected, we set a variable indicating that
we're now connected, and right before we send Disconnected, we unset said
variable.

Then, when we receive a request from Zelden to join a channel or send a message
or things like that, we check to see if that var saying we're connected says
we're connected, and if not, we either ignore it or log to the server log that
we're not connected, not sure which yet. If we are connected, though, then we
convert it into an appropriate IRC message and send it off on its merry way.

I think that should be just about it for now.

(Zelden's going to do the work of keeping track of the current list of users at
any given channel, so we don't need to worry about that. I will probably have
that as a separate library that can be used independently from Zelden, though,
so that IRC bots can use that library on top of this one without using the rest
of Zelden, and Zelden can use it to 1: keep track of current state, and maybe,
depending on how I store things, 2: use it to run simulations of what a channel
might have looked like at a particular time. #2 will be something I'll probably
wait on, though, so I'll just plan on #1 for now.)

Also, it might be a good idea to be able to pass to our little read function
a custom timeout variable (or multiple timeout variables) to time out on in
addition to the one it creates. This would allow an overall timeout for how
long the handshake can take to be created just before the handshake starts and
passed throughout the handshake. Also might want to create a ReaderT on the
queue and endpoint for reading/writing from/to the socket, which would make the
read function even simpler; could also have the ReaderT contain a list of
timeout variables for read to additionally make use of, as well as the amount
of time that the timeout it creates should be for. Probably should use a custom
data type for this.

We ought also to have the logic for processing ordinary everyday messages from
the connection be inside MaybeT as well, so that the few messages that require
us to stop and wait for other messages (such as the initial topic on a join; we
wait for the part that indicates who initially set it) can just operate as
typical calls to read and still abort properly when we're asked to disconnect.

Ok, need to redo the message reading code a bit.

First off, we need to be able to handle pings and respond to them with pongs.
I'm pretty certain they're the only messages that can happen sporadically.

Actually, maybe we should have something that's part of the session that stores
a list of functions waiting to trap messages coming in from the server...

Although that's probably too much for now. So how about we just write a simple
function that reads from the action queue and from the event queue and stuff.

Although we still need to read from the action queue even when we're not
connected to a server...

Maybe we should have some sort of state somewhere that doesn't get erased when
we abort with EitherT...


-}

readMessage :: IRCM Message
readMessage = do
    session <- ask
    -- Make a timeout variable to time out reads that take too long
    -- TODO: Pretty sure this can be collapsed into one line using Maybe as a
    -- monad (or MaybeT)
    timeoutVar <- liftIO $ case sessionReadTimeout session of
        (Just timeout) -> liftM Just $ makeTimeout timeout
        Nothing        -> return Nothing
    -- Read the next message
    maybeMessage <- liftIO $ atomically $ do
        -- Try to read a message
        message <- tryReadEndpoint $ sessionReader session
        -- See if we've timed out yet, if a timeout was specified
        timedOut <- case timeoutVar of
            (Just v) -> readTVar v
            Nothing  -> return False
        -- See if we're still enabled
        enabled <- readTVar $ connEnabledVar $ sessionConnection session
        case message of
            -- If we got a message, return it
            (Just m) -> return $ Right m
            -- We didn't get a message, so see if we've timed out yet
            Nothing  -> if timedOut
                -- Timed out
                then return $ Left ReadTimedOut
                else if not enabled
                -- Disabled
                then return $ Left ConnectionDisabled
                -- Didn't time out; go back to waiting for a message
                else retry
    -- Now see if we got a message or if we timed out
    case maybeMessage of
        -- Got a message; return it
        (Right m)     -> return m
        -- Timed out; bail out of the IRC session
        (Left reason) -> bail reason

-- | Wait for a message to arrive, then return it without removing it from the
-- queue.
peekMessage :: IRCM Message
peekMessage = do
    message <- readMessage
    session <- ask
    unGetEndpoint (sessionReader session) message
    return message

getConfig :: String -> String -> IRCM String
getConfig name def = do
    session <- ask
    return $ M.findWithDefault def name $ connConfigVars $ sessionConnection session

runSession :: IRCM ()
runSession = do
    -- Write the user message. We'll worry about allowing the user to configure
    -- the various parameters later.
    writeMessage $ Message Nothing "USER" $ replicate 4 "zelden"
    -- Write a NICK message, then peek the next message. If it's a
    -- nick-already-in-use message, read it off the queue and try again.
    let chooseNick nextNumber = do
        -- Send the nick command
        let nickToTry = "zelden" ++ fromMaybe "" (liftM show nextNumber)
        writeMessage $ Message Nothing "NICK" [nickToTry]
        -- See if the response is 433 (Nick already in use). TODO: Might also
        -- want to check for invalid nick chars here too, and bail if that's
        -- what we get
        response <- peekMessage
        case response of
            (Message _ "433" _) -> do
                -- Already in use. Read the already-in-use message that we
                -- just peeked
                readMessage
                -- Then try again, with a number appended or incremented, or
                -- bail if we've already tried 20 times. TODO:
                -- ought to make the list of nicks to try configurable, and
                -- resort to incrementing numbers if none of them work.
                if (fromMaybe 0 nextNumber) > 20
                    then bail TooManyNickTries
                    else chooseNick $ fromMaybe 1 $ liftM (+ 1) nextNumber
            _ -> do
                -- It worked! Issue the Connected event with the nick that
                -- made it (and store the nick locally), then move on.
                session <- getSession
                writeVar (sessionNick session) nickToTry
                sendEvent $ Connected nickToTry
                return ()
    chooseNick Nothing
    -- Now read off messages until we hit a 376 End of MOTD, and process each
    -- one as we read it.
    let readToMotd = do
        message <- readMessage
        case message of
            -- If it's a 376, we're done here
            (Message _ "376" _) -> return ()
            -- If it's 005, then it's server info, so parse and store it
            (Message _ "005" (_:options')) -> do
                -- Strip the "are supported by this server" bit
                let options = init options'
                -- Split the options up, and stick each one into the options map
                forM options $ \option -> do
                    -- Store no-value options with an empty string value
                    k:vs <- splitOn "=" option
                    let v = fromMaybe "" $ listToMaybe vs
                    session <- getSession
                    modifyVar (sessionVars session) $ M.insert k v
                readToMotd
            -- If it's anything else, ignore it for now
            _ -> return ()
    readToMotd
    -- We're now alive and running! Now we sit in a loop and try to read
    -- messages from the server and read actions that we need to run.
    forever $ do
        -- TODO: Pick up here. We need a way to read messages in STM, or
        -- perhaps a readMessage-like thing for also reading actions from the
        -- list of actions to be processed. Or maybe have them share the same
        -- queue in some sort of manner and just read from that. Needs thought.



-- | Bail out of an IRC session. We do this when the remote end closes its
-- connection or when we're told to disable ourselves, or things like that.
bail :: DisconnectReason -> IRCM ()
bail reason = lift $ EitherT $ return $ Left reason

bailIf :: DisconnectReason -> Bool -> IRCM ()
bailIf reason test = if test then bail reason else return ()

getSession :: IRCM IRCSession
getSession = ask

sendEvent :: Event -> IRCM ()
sendEvent event = do
    session <- ask
    liftIO $ (connHandler $ sessionConnection session) event 

writeMessage :: Message -> IRCM ()
writeMessage message = do
    -- We could check if the connection's been disabled before writing, but
    -- a write will essentially always be followed by a read at some point, so
    -- there's not much point.
    session <- ask
    liftIO $ atomically $ writeQueue (sessionWriter session) message


instance Connection IRCConnection






















{-
If we're enabled, try to connect, and if we do, enter the loop that processes
connection stuff. More later.

Once we disconnect, or if we're not enabled, read and process actions until we
either get an Enabled action, in which case start over after changing enabled
to true, or we get a Shutdown action, in which case bail out of the whole
thing.
-}

run2 :: Endpoint Action -> (Event -> IO ()) -> ContT () IO ()
run2 actionQueue actionEndpoint eventHandler enabledVar = loop $ \continueOuter breakOuter -> do
    enabled <- atomically $ readTVar enabledVar
    when enabled $ callCC \bailConnection -> do
        maybeSocket <- openSocket "irc.opengroove.org" 6667 actionQueue
        when (not $ isJust maybeSocket) bailConnection
        let (Just outputQueue) = maybeSocket
        queue endpoint
        
    action <- atomically $ readEndpoint actionEndpoint
    case action of
        Enable -> undefined


data IRCConnection2
    = IRCConnection2 {
        inEndpoint :: Endpoint Message,
        outQueue :: Queue Message,
        currentNick :: String
    }

data Thing = Timeout | M Message | A Action
    deriving (Read, Show)

run3 :: Queue (Either Message Action) ->  -> (Event -> IO ()) -> ContT () IO ()
run3 actionEndpoint handleEvent = do
    connVar <- atomically $ newTVar (Nothing :: Maybe IRCConnection2)
    messageEndpointVar <- atomically $ newTVar (Nothing :: Maybe (Endpoint Message))
    enabledVar <- atomically $ newTVar False
    connectTimeoutVar <- atomically $ newTVar True >>= newTVar
    loop $ \continueOuter breakOuter -> do
        enabled <- atomically $ readTVar enabledVar
        conn <- atomically $ readTVar connVar
        nextThing <- atomically $ do
            timedOutVar <- readTVar connectTimeoutVar
            timedOut <- readTVar timedOutVar
            cm <- readTVar connVar
            if timedOut
                then do
                    delayVar <- registerDelay 20000000 -- 20 seconds. TODO: Make this configurable
                    return Timeout                                        -- 
                else liftM A (readEndpoint actionEndpoint) `orElse` liftM M (maybe retry (readEndpoint . inEndpoint) cm)
        case (nextThing, conn) of
            (Timeout, Nothing) -> undefined -- TODO: Connect here. Also have
                -- some sort of timeout support for issuing ISON messages, but
                -- probably have a separate timeout to allow ISON messages to
                -- delay longer or something. Also consider having watchlist
                -- support in some manner, maybe watchlist all (or as many as
                -- we can) of the user's contacts, which would probably require
                -- Zelden to somehow give us that information. 
            (A (Action _ (JoinRoom room)), Just c) -> do
                writeQueue $ Message Nothing "JOIN" [room]
            (A (Action _ (PartRoom room)), Just c) -> do
                writeQueue $ Message Nothing "PART" [room]
            (A (Action _ (SendUserMessage user message)), Just c) -> do
                writeQueue $ Message Nothing "PRIVMSG" [user, message]
            (A (Action _ (SendRoomMessage room message)), Just c) -> do
                writeQueue $ Message Nothing "PRIVMSG" [room, message]
            (A (Action _ (SwitchSelfKey key)), Just c) -> do
                writeQueue $ Message Nothing "NICK" [key]
            (A (Action _ (SetRoomTopic room topic)), Just c) -> do
                writeQueue $ Message Nothing "TOPIC" [room, topic]
            (M (Message (Just (NickName oldNick _ _)) "NICK" [newNick]), Just c)
                | oldNick == currentNick c -> do
                    -- Self rename
                    atomically $ writeTVar connVar $ c {currentNick=newNick}
                    handleEvent $ Event M.empty $ UserSwitchedKey oldNick newNick
                | otherwise -> do
                    -- Someone else renamed
                    handleEvent $ Event M.empty $ UserSwitchedKey oldNick newNick
            (M (Message (Just (NickName fromNick _ _)) "JOIN" [room]), Just c) -> do
                -- Us or someone else joined
                handleEvent $ Event M.empty $ UserJoinedRoom room fromNick
            (M (Message (Just (NickName fromNick _ _)) "PART" room:maybeReason), Just c) -> do
                -- Us or someone else parted a room
                handleEvent $ Event M.empty $ UserPartedRoom room fromNick $ Parted $ fromMaybe "" $ listToMaybe maybeReason
            (M (Message (Just (NickName fromNick _ _)) "QUIT" maybeReason), Just c) -> do
                handleEvent $ Event M.empty $ UserQuit fromNick $ fromMaybe "" $ listToMaybe maybeReason
            (M (Message (Just (NickName fromNick _ _)) "PRIVMSG" [recipient, message]), Just c)
                | recipient == currentNick c -> do
                    -- Direct message to us
                    handleEvent $ Event M.empty $ UserMessage fromNick message
                | otherwise -> do
                    -- Message to a room. TODO: Might need to double-check here
                    -- that it's actually a valid room, as I seem to recall
                    -- that some servers will deliver messages with weird
                    -- recipients when snooping in on direct messages when an
                    -- op...
                    handleEvent $ Event M.empty $ RoomMessage recipient fromNick message
            (t, cm) -> putStrLn $ "IRC: Unhandled thing from connection " ++ show cm ++ ": " ++ show t


waitForTrue :: a -> TVar Bool -> STM a
waitForTrue constant var = do
    value <- readTVar var
    if value
        then return constant
        else retry









































