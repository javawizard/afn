
module Zelden.Server where

import Zelden.IO
import Database.HDBC as DB
import qualified Data.Map as M


data ConnectionBox = ConnectionBox (forall a. Connection a => a)
data ProtocolBox = ProtocolBox (forall a. Protocol a => a)


data Event = Event EventData (M.Map String String)
type RoomKey = String
type UserKey = String

data EventData
    = Connected
    | Disconnected
    | JoinedRoom RoomKey
    | PartedRoom RoomKey
    | RoomTopic RoomKey String
    | RoomMessage RoomKey UserKey String
    | UserMessage UserKey String
    | UserStatus UserKey UserStatus
    | SelfUserKey UserKey
    | SwitchUserKey UserKey UserKey

data UserStatus
    = Available
    | Away
    | Busy
    | Offline

-- | Alias for (,) that allows conveniently writing dictionaries as
-- M.fromList ["a" := "b", "c" := "d"].
(:=) :: a -> b -> (a, b)
(:=) = (,)
infixl 1 :=



type EventCallback = ConnectionBox -> Event -> IO ()

class Protocol a where
    getProtocolName :: a -> String
    initProtocol :: a -> IO ()
    likesPastebin :: a -> Bool
    createConnection :: EventCallback -> IO ConnectionBox

class Connection a where
    -- getProtocol :: a -> ProtocolBox
    setParam :: a -> String -> String -> IO ()
    enable :: a -> IO ()
    disable :: a -> IO ()
    

-- | A class of types that can receive events from connections. Connections are
-- given a Controller to which they indicate events that happen.
class Controller a where
    





type DBM a = ReaderT DB.Connection IO a

type DBAction = forall a. DBAction (DBM a) (TMVar a)

closeDB :: DBM ()
closeDB = ask >>= lift . DB.close














    