
import Control.Monad.Trans (liftIO)
import Test.WebDriver
import Test.WebDriver.Commands
import Data.Text (pack, unpack)

login :: String -> [(String, String)] -> String -> WD ()
login username questions password = do
    openPage "https://my.ucreditu.com/"
    findElem $ ById "UsernameField" >>= sendKeys $ pack username
    findElem $ ById "SubmitNext" >>= click
    findElem $ ById "PasswordField" >>= sendKeys $ pack password
    findElem $ ById "SubmitNext" >>= click
    bodyText <- findElem $ ByCSS "body" >>= getText
    let (_, answer) = filter (\q -> fst q `isInfixOf` bodyText) questions !! 0
    findElem $ ById "Answer" >>= sendKeys $ pack answer
    findElem $ ById "SubmitNext" >>= click

