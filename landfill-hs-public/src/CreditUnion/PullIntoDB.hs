
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
import Data.Maybe (fromJust)
import qualified Control.Exception as E

main = do
    args <- getArgs
    db <- connectSqlite3 $ args !! 0
    waitTime <- readIO (args !! 1) :: IO Int
    run db "create table if not exists balance (person text, shareid text, sharename text, checktime integer, available integer, total integer)" []
    commit db
    -- [(username, [(q, a), (q, a), (q, a)]), (username, [...]), ...]
    accountInfos <- liftM read $ readFile "/home/jcp/ucreditu/questions" :: IO [(String, [(String, String)])]
    passwords <- forM accountInfos $ \(username, _) -> do
        putStrLn $ "Account " ++ username ++ ":"
        password <- getPass'
        return (username, password)
    putStrLn "Running."
    let loop = do
        flip E.catch (\e -> putStrLn $ "Exception happened: " ++ show (e :: E.SomeException)) $ do
            time <- (liftM (floor . realToFrac) getPOSIXTime) :: IO Integer
            putStrLn $ "Starting at time: " ++ show time 
            forM_ accountInfos $ \(username, questions) -> do
                putStrLn "Starting session"
                accounts <- runSession defaultSession defaultCaps $ do
                    liftIO $ putStrLn $ "Logging into " ++ show username
                    login username questions $ fromJust $ lookup username passwords
                    liftIO $ putStrLn "Getting accounts"
                    accounts <- getAccounts
                    logout
                    return accounts
                threadDelay $ 3000*1000
                putStrLn "Inserting into database"
                forM_ accounts $ \(shareName, _, number, available, total) -> do
                    (run db "insert into balance (person, shareid, sharename, checktime, available, total) values (?, ?, ?, ?, ?, ?)" $
                        [toSql username, toSql $ drop 2 $ dropWhile (/= '-') number, toSql shareName, toSql time,
                         toSql (read $ filter (flip elem "0123456789") available :: Integer), toSql (read $ filter (flip elem "0123456789") total :: Integer)])
            commit db
            putStrLn "Done"
        flip E.catch (\e -> putStrLn $ "Exception happened while killing firefox: " ++ show (e :: E.SomeException)) $ system "pkill -f firefox" >> return ()
        threadDelay $ waitTime * 1000000
        loop
    loop
