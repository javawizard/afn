{-# LANGUAGE ExistentialQuantification, RankNTypes #-} 

module Zelden.Server where

import Zelden.IO
-- import Database.HDBC as DB
import qualified Data.Map as M
import Control.Monad.Trans.Reader


data ConnectionBox = ConnectionBox (forall a. Connection a => a)
data ProtocolBox = ProtocolBox (forall a. Protocol a => a)


data Event = Event EventData (M.Map String String)
type RoomKey = String
type UserKey = String

data EventData
    -- | Connection just connected. This shouldn't actually be issued until
    -- 
    = Connected UserKey
    -- | Connection was disconnected.
    | Disconnected
    -- | User joined room, possibly us or someone else. Note that this can
    -- happen without a request that the connection join a room if the server
    -- forces us to join a room (IRC is one such protocol that can do this).
    | UserJoinedRoom RoomKey UserKey
    -- | User left room, possibly us or someone else. Note that this is one of
    -- two ways a user can leave a room; the other is with UserQuit, which
    -- removes the user from all rooms they were joined to. 
    | UserPartedRoom RoomKey UserKey PartReason
    -- | User quit, possibly us or someone else. If it's us, this will almost
    -- certainly be followed by a Disconnect event. Note that we can end up
    -- quitting without trying if, say, we ping out of the server for some
    -- reason. (A local pingout just results in a Disconnect, without a
    -- corresponding UserQuit. TODO: Actually, should we have a UserQuit for
    -- ourselves on a local pingout? Probably protocol-specific though.)
    | UserQuit UserKey String
    -- | Topic for the specified room was modified. This also happens right
    -- after a UserJoinedRoom on ourselves, indicating the room's current
    -- topic. Information about the time the topic was last set and by whom
    -- should go into the event's extra parameters, although I might move it
    -- to RoomTopic if it's used enough. (It would almost certainly be wrapped
    -- in a Maybe, though, as topic changes don't mention what time they were
    -- set at or any such thing.)
    | RoomTopic RoomKey UserKey String
    -- | Message was sent to a room. I might merge this with UserMessage in the
    -- future, and have a Maybe RoomKey parameter that's Nothing for users and
    -- Just roomKey for rooms.
    | RoomMessage RoomKey UserKey String
    -- | Private message to us from the specified user. I'm thinking IRC
    -- notices will just be UserMessages (and RoomMessages) with special keys
    -- in the event's extra parameters, as they're almost exclusive to IRC. I
    -- also haven't decided how I want to handle /me actions yet.
    | UserMessage UserKey String
    -- | User's status changed. I haven't decided whether our own status
    -- changes should be reported with this. When a protocol connects, all
    -- users should be assumed to have a status of Offline; UserStatus messages
    -- will be sent by the connection as soon as it knows the statuses of any
    -- users that it knows about. Note that, on some protocols, offline users
    -- can still send user and room messages.
    | UserStatus UserKey UserStatus
    -- | We know our user key now. This must be sent relatively quickly after
    -- Connected, and before any other events that use UserKeys; not doing so
    -- will result in lots of exceptions within Zelden itself. I'm still
    -- deciding whether this or UserSwitchedKey will be issued when our key
    -- changes while we're still connected.
    | SelfUserKey UserKey
    -- | Another user that we previously knew under the first key is now known
    -- under the second, and any further events mentioning the user will use
    -- the second key. It's up to the user of the connection whether it wants
    -- to start tracking the user under the second key or just log the event
    -- under the first key and treat the user as a new user. (Zelden will
    -- likely have a configuration option for choosing between the two
    -- behaviors. Most contemporary IRC clients seem to use the former
    -- behavior; I generally tend to prefer the latter.)
    | UserSwitchedKey UserKey UserKey

data UserStatus
    = Available
    -- | User is away. TODO: add some sort of reason to this, or merge Away and
    -- Busy into Unavailable, which has a message and a reason (of type
    -- UnavailableReason or some such, whose constructors would be things like
    -- Away, Busy, etc.)
    | Away
    | Busy
    | Offline

-- | Reasons that a user (us or someone else) left a room. This does not
-- include the user disconnecting from the server altogether; such things are
-- a completely separate event, namely UserQuit.
data PartReason
    -- | User left the room
    = Parted String
    -- | User was kicked by the specified user for the specified reason
    | Kicked UserKey String

-- | Alias for (,) that allows conveniently writing dictionaries as
-- M.fromList ["a" := "b", "c" := "d"].
-- (:=) :: a -> b -> (a, b)
-- (:=) = (,)
-- infixl 1 :=



type EventCallback = ConnectionBox -> Event -> IO ()

class Protocol a where
    getProtocolName :: a -> String
    initProtocol :: a -> IO ()
    likesPastebin :: a -> Bool
    createConnection :: a -> EventCallback -> IO ConnectionBox

class Connection a where
    -- getProtocol :: a -> ProtocolBox
    setParam :: a -> String -> String -> IO ()
    enable :: a -> IO ()
    disable :: a -> IO ()






    

-- | A class of types that can receive events from connections. Connections are
-- given a Controller to which they indicate events that happen.
class Controller a where
    





-- type DBM a = ReaderT DB.Connection IO a

-- type DBAction = forall a. DBAction (DBM a) (TMVar a)

-- closeDB :: DBM ()
-- closeDB = ask >>= lift . DB.close














    