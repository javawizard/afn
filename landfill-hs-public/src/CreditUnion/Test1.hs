
module CreditUnion.Test1 where
import CreditUnion.Client (login, getAccounts, getPass)
import System.IO (readFile)
import Control.Monad (liftM)
import Test.WebDriver
import Control.Monad.Trans (liftIO)

main :: do
    questions <- liftM read $ readFile "/home/jcp/ucreditu/questions"
    pass <- getPass'
    runSession defaultSession defaultCaps $ do
        login "javawizard" questions pass
        accounts <- getAccounts
        liftIO $ putStrLn $ show accounts
    


