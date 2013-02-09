
module CreditUnion.PullIntoDB where

import CreditUnion.Client (login, getAccounts, getPass', logout)
import System.IO (readFile)
import Control.Monad (liftM)
import Test.WebDriver
import Control.Monad.Trans (liftIO)
import Database.HDBC
import Database.HDBC.Sqlite3
import System (getArgs)

main = do
    args <- getArgs
    db <- connectSqlite3 $ args !! 0
    password <- getPass'
    questions <- liftM read $ readFile "/home/jcp/ucreditu/questions"
    putStrLn "Running."
    let loop = do
        putStrLn "Starting session"
        accounts <- runSession defaultSession defaultCaps $ do
            login "javawizard" questions pass
            accounts <- getAccounts
            logout
            return accounts
        putStrLn "Inserting into database"
        
        putStrLn "Done"
        threadDelay (1800*1000*1000)
        loop