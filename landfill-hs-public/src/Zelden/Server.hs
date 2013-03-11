{-# LANGUAGE ExistentialQuantification, RankNTypes #-} 

module Zelden.Server where

import Zelden.IO
-- import Database.HDBC as DB
import qualified Data.Map as M
import Control.Monad.Trans.Reader


data ConnectionBox = ConnectionBox (forall a. Connection a => a)
data ProtocolBox = ProtocolBox (forall a. Protocol a => a)



type EventCallback = ConnectionBox -> Event -> IO ()


data Manager

type ViewNumber



-- type DBM a = ReaderT DB.Connection IO a

-- type DBAction = forall a. DBAction (DBM a) (TMVar a)

-- closeDB :: DBM ()
-- closeDB = ask >>= lift . DB.close














    