
module Landfill.Bindings2 where

import Data.Sequence as S

class ReadValue a v where
    listen :: (Ord k) => a -> k -> (v -> v -> IO ()) -> IO ()
    unlisten :: (Ord k) => a -> k -> IO ()

class WriteValue a b where
    writeValue :: a -> b -> IO ()

data ValueBinder a = ...

instance ReadValue (ValueBinder a) a where
    listen binder key function = do
        undefined
    unlisten binder key = do
        undefined

instance WriteValue (ValueBinder a) a where
    writeValue binder value = do
        undefined

































