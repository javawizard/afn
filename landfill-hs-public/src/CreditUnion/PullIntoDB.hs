
module CreditUnion.PullIntoDB where

import CreditUnion.Client (login, getAccounts, getPass', logout)
import System.IO (readFile)
import Control.Monad (liftM)
import Test.WebDriver
import Control.Monad.Trans (liftIO)
import Database.HDBC
import Database.HDBC.Sqlite3
import System.Environment (getArgs)
import Data.Time.Clock.POSIX
import System.Process (system)
import Control.Concurrent
import Control.Monad

main = do
    args <- getArgs
    db <- connectSqlite3 $ args !! 0
    run db "create table if not exists balance (person text, shareid text, sharename text, checktime integer, available integer, total integer)" []
    commit db
    password <- getPass'
    questions <- liftM read $ readFile "/home/jcp/ucreditu/questions"
    putStrLn "Running."
    let loop = do
        flip catch (\e -> putStrLn $ "Exception happened: " ++ show e) $ do
            putStrLn "Starting session"
            accounts <- runSession defaultSession defaultCaps $ do
                login "javawizard" questions password
                accounts <- getAccounts
                logout
                return accounts
            threadDelay $ 3000*1000
            system "pkill -f firefox"
            putStrLn "Inserting into database"
            time <- (liftM (floor . realToFrac) getPOSIXTime) :: IO Integer
            forM_ accounts $ \(shareName, _, number, available, total) -> do
                (run db "insert into balance (person, shareid, sharename, checktime, available, total) values (?, ?, ?, ?, ?, ?)" $
                    [toSql "javawizard", toSql $ drop 2 $ dropWhile (/= '-') number, toSql shareName, toSql time,
                     toSql (read $ filter (flip elem "0123456789") available :: Integer), toSql (read $ filter (flip elem "0123456789") total :: Integer)])
            commit db
            putStrLn "Done"
        threadDelay (20*1000*1000)
        loop
    loop
