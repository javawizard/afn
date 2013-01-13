
module Zelden.JSON where

import qualified Data.Map as M
import qualified Text.JSON as J

data JSValue
    = JSNull
    | JSBool Bool
    | JSRational Bool Rational
    | JSString String
    | JSArray [JSValue]
    | JSObject (M.Map String JSValue)

data JSONError
    = NoSuchKey String

type JSReadM a = StateT JSValue (Either JSONError) a

getValue :: String ->  JSReadM JSValue
getValue

getBool :: JSReadM JSValue