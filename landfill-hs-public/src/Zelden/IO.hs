
module Zelden.IO where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.SaneTChan
import Network

streamSocket :: Handle -> (String -> i) -> (o -> String) -> Queue (Maybe i) -> Endpoint (Maybe o) -> IO ()


