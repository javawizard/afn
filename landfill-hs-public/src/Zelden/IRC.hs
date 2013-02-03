
module Zelden.IRC where

import Zelden.Protocol
import Control.Concurrent.STM
import Control.Concurrent.STM.SaneTChan

data IRCProtocol = IRCProtocol

data IRCConnection
    = IRCConnection {
        
    }

instance Protocol IRCProtocol where
    likesPastebin _ = True
    createConnection _ callback = do
        return IRCConnection

{-
So, how are we going to actually do connections?
-}

instance Connection IRCConnection where

