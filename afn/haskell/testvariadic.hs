{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
import Data.List (concat, intersperse)

class HelloType a where
    hello_internal :: [String] -> String -> a

instance HelloType String where
    hello_internal list name = "Hello " ++ (concat $ intersperse " and " (list ++ [name])) ++ "! How are you?"

instance (HelloType r) => HelloType (String -> r) where
    hello_internal list name s = hello_internal (list ++ [name]) $ s

hello :: (HelloType a) => String -> a
hello name = hello_internal [] name

