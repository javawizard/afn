
module Zelden.Server where

import Zelden.IO


data ConnectionBox = ConnectionBox (forall a. Connection a => a)
data ProtocolBox = ProtocolBox (forall a. Protocol a => a)

data Event
    = Connected
    | Disconnected
    | MessageReceived MessageInfo
    | JoinedChannel String
    | LeftChannel String
    -- Add lots more events and stuff

type EventCallback = ConnectionBox -> Event -> IO ()

class Protocol a where
    getProtocolName :: a -> String
    initProtocol :: a -> IO ()
    likesPastebin :: a -> Bool
    createConnection :: ProtocolConfig -> EventCallback -> IO ConnectionBox

class Connection a where
    getProtocol :: a -> ProtocolBox
    connect :: a -> IO ()
    sendMessage :: a
    