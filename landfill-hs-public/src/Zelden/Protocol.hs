{-# LANGUAGE ExistentialQuantification, RankNTypes #-}

module Zelden.Protocol where

import Zelden.IO
-- import Database.HDBC as DB
import qualified Data.Map as M
import Control.Monad.Trans.Reader 

data ConnectionBox = ConnectionBox (forall a. Connection a => a)
data ProtocolBox = ProtocolBox (forall a. Protocol a => a)


data Event = Event (M.Map String String) EventData deriving (Show)
type RoomKey = String
type UserKey = String

data EventData
    -- | Connection just connected. Single parameter is our own user key. This
    -- shouldn't be issued until we actually know what our user key is.
    = Connected UserKey
    -- | Connection was disconnected.
    | Disconnected
    -- | We just joined a room, whether because we chose to (by sending a
    -- JoinRoom) or because the server forced us into a particular room.
    -- If the room has a topic, it will be provided, along with the user that
    -- set the topic and the time it was set. (Right now, protocols that don't
    -- have any way of indicating the user that set the topic or the time
    -- should just use a key invalid to that protocol ("unknown.user" usually
    -- suffices) and 0, respectively.) The list of users currently present at
    -- the room will also be provided.
    | SelfJoinedRoom RoomKey (Maybe (String, UserKey, Integer)) [UserKey]
    -- | Another user joined a room that we're currently joined to. Note that
    -- users currently present at the room when we join are instead indicated
    -- in the SelfJoinedRoom message.
    | UserJoinedRoom RoomKey UserKey
    -- | User left room, possibly us or someone else. Note that this is one of
    -- two ways a user can leave a room; the other is with UserQuit, which
    -- removes the user from all rooms they were joined to. 
    | SelfPartedRoom RoomKey PartReason
    | UserPartedRoom RoomKey UserKey PartReason
    -- | User quit, possibly us or someone else. If it's us, this will almost
    -- certainly be followed by a Disconnect event. Note that we can end up
    -- quitting without trying if, say, we ping out of the server for some
    -- reason. (A local pingout just results in a Disconnect, without a
    -- corresponding UserQuit. TODO: Actually, should we have a UserQuit for
    -- ourselves on a local pingout? Probably protocol-specific though.)
    -- Things using protocols shouldn't pack up and go home when they receive a
    -- UserQuit for ourselves; they should instead wait for the Disconnect
    -- that should immediately follow. (But they're free to log the UserQuit
    -- as desired.)
    | UserQuit UserKey String
    -- | Topic for the specified room was modified, or set for the first time
    -- if the room did not have a topic when we joined it. (Topics that were
    -- set before we joined the room are passed as part of the SelfJoinedRoom
    -- event.)
    | RoomTopic RoomKey UserKey String
    -- | Message was sent to a room. I might merge this with UserMessage in the
    -- future, and have a Maybe RoomKey parameter that's Nothing for users and
    -- Just roomKey for rooms.
    | RoomMessage RoomKey UserKey String
    -- | Private message to us from the specified user. I'm thinking IRC
    -- notices will just be UserMessages (and RoomMessages) with special keys
    -- in the event's extra parameters, as they're almost exclusive to IRC. I
    -- also haven't decided how I want to handle /me actions yet. Also, not
    -- sure if/how I want to handle confirmations from protocols such as XMPP
    -- that a message was actually sent, probably will just have those be a
    -- general event that can be used to log general protocol events.
    | UserMessage UserKey String
    -- | User's status changed. I haven't decided whether our own status
    -- changes should be reported with this. When a protocol connects, all
    -- users should be assumed to have a status of Offline; UserStatus messages
    -- will be sent by the connection as soon as it knows the statuses of any
    -- users that it knows about. Note that, on some protocols, offline users
    -- can still send user and room messages. Also note that some protocols may
    -- send UserStatus Offline events on connect for users that are offline but
    -- that still have a status message set. (The last parameter is the status
    -- message of the user. Additional information, such as the user's mood on
    -- protocols that support it, should go into the event's extra parameters.)
    | UserStatus UserKey UserStatus (Maybe String)
    -- | A user (possibly us or someone else) that we previously knew under the
    -- first key is now known under the second, and any further events
    -- mentioning the user will use the second key. It's up to the user
    -- of the connection whether it wants to start tracking the user
    -- under the second key or just log the event under the first
    -- key and treat the user as a new user. (Zelden will likely
    -- have a configuration option for choosing between the two
    -- behaviors. Most contemporary IRC clients seem to use the former
    -- behavior; I generally tend to prefer the latter.)
    | UserSwitchedKey UserKey UserKey
    deriving (Show)

data UserStatus
    -- | User is available.
    = Available
    -- | User is away. Not yet sure how the date they went away should be
    -- reported; for now, it'll go into the event's extra parameters. TODO:
    -- Should statuses have extra data associated with them instead of just
    -- data tacked onto the event? Then things like the connection manager
    -- facility I want to write could store a status's extra information along
    -- with the status itself.
    | Away
    -- | User is busy and does not wish to be contacted.
    | Busy
    -- | User is offline.
    | Offline
    deriving (Show)

-- | Reasons that a user (us or someone else) left a room. This does not
-- include the user disconnecting from the server altogether; such things are
-- a completely separate event, namely UserQuit.
data PartReason
    -- | User left the room
    = Parted String
    -- | User was kicked by the specified user for the specified reason
    | Kicked UserKey String
    deriving (Show)

data Action = Action (M.Map String String) ActionData
    deriving (Show)

data ActionData
    -- | Joins a room, if the protocol is currently connected.
    = JoinRoom RoomKey
    -- | Parts a room with a particular reason
    | PartRoom RoomKey String
    -- | Sends a message to a specific user.
    | SendUserMessage UserKey String
    -- | Sends a message to a specific room.
    | SendRoomMessage RoomKey String
    -- | Switch our own key. Not (at present) guaranteed to work when not
    -- connected, and certainly not guaranteed to work on all protocols (IRC
    -- is the only one to support this right now). UserSwitchedKey will be
    -- issued when the change actually happens.
    | SwitchSelfKey UserKey
    -- | Changes the room's topic. RoomTopic will be issued once (if) the
    -- change is actually made.
    | SetRoomTopic RoomKey String
    -- | Enables the protocol. The protocol should connect as soon as possible.
    | Enable
    -- | Disables the protocol. The protocol should disconnect immediately.
    | Disable
    -- | Shuts down the protocol. The protocol should release all resources
    -- associated with it and stop any threads it may have started.
    | Shutdown
    -- | Sets a protocol parameter.
    | SetParam String String
    deriving (Show)

-- Alias for (,) that allows conveniently writing dictionaries as
-- M.fromList ["a" := "b", "c" := "d"].
-- (:=) :: a -> b -> (a, b)
-- (:=) = (,)
-- infixl 1 :=



type EventCallback = Event -> IO ()

-- I ended up simplifying Protocol and Connection down to a simple message
-- passing construct, with Actions being sent in and Events being sent out.
-- At this point, it might be a good idea to factor them out into simple data
-- types encapsulating functions instead of having them be full-blown classes.
-- That would also get rid of the need for things like ConnectionBox.

class Protocol a where
--    getProtocolName :: a -> String
--    initProtocol :: a -> IO ()
    likesPastebin :: a -> Bool
    createConnection :: a -> EventCallback -> IO ConnectionBox

class Connection a where
    -- getProtocol :: a -> ProtocolBox
    sendAction :: a -> Action -> IO ()




