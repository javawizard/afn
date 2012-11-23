{-# LANGUAGE OverloadedStrings #-}

module CreditUnion.Client where

import Control.Monad.Trans (liftIO)
import Test.WebDriver
import Test.WebDriver.Commands
import Data.Text (pack, unpack)
import Data.List (isInfixOf)
import Control.Concurrent (threadDelay)
import System.Console.Haskeline

getPass :: String -> IO String
getPass text = do
    (Just pass) <- runInputT defaultSettings $ getPassword Nothing text
    return pass

login :: String -> [(String, String)] -> String -> WD ()
login username questions password = do
    openPage "https://my.ucreditu.com/"
    liftIO $ threadDelay (5*1000*1000)
    findElem (ById "UsernameField") >>= (sendKeys $ pack username)
    findElem (ById "SubmitNext") >>= click
    findElem (ById "PasswordField") >>= (sendKeys $ pack password)
    findElem (ById "SubmitNext") >>= click
    bodyText <- findElem (ByCSS "body") >>= getText >>= return . unpack
    let (_, answer) = filter (\q -> fst q `isInfixOf` bodyText) questions !! 0
    findElem (ById "Answer") >>= (sendKeys $ pack answer)
    findElem (ById "SubmitNext") >>= click

