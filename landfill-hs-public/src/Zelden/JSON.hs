
module Zelden.JSON where

import qualified Data.Map as M
import qualified Text.JSON as J

data JSValue
    = JSNull
    | JSBool Bool
    | JSNumber Integer -- Not using floating point numbers in Zelden right now
    | JSString String
    | JSArray [JSValue]
    | JSObject (M.Map String JSValue)

data Type = NullType | BoolType | NumberType | StringType | ArrayType | ObjectType

data JSONError
    = NoSuchKey String
    | NeededType Type

getValue :: JSValue -> String -> Either JSONError JSValue
getValue (JSObject m) k = case (M.lookup k m) of
    Nothing -> Left $ NoSuchKey k
    Just v  -> Right v
getValue _ k = Left $ NeededType ObjectType

getElements :: JSValue -> Either JSONError [JSValue]
getElements (JSArray a) -> Right a
getElements _ -> Left $ NeededType ArrayType

getNumber :: JSValue -> Either JSONError Integer
getNumber (JSNumber n) -> Right n
getNumber _ -> Left $ NeededType NumberType

getString :: JSValue -> Either JSONError String
getString (JSString s) -> Right s
getString _ -> Left $ NeededType NumberType

getBool :: JSValue -> Either JSONError Bool
getBool (JSBool b) -> Right b
getBool _ -> Left $ NeededType BoolType

-- | Gets the string value of the specified key in the specified object.
getString' :: JSValue -> String -> Either JSONError String
getString' v k = liftM getString $ getValue v k

getNumber' :: JSValue -> String -> Either JSONError Integer
getNumber' v k = liftM getNumber $ getValue v k

getBool' :: JSValue -> String -> Either JSONError Bool
getBool' v k = liftM getBool $ getValue v k












