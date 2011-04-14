
module Jpath1 where
import Text.ParserCombinators.Parsec


tNumDecimalPoint :: Parser String
tNumDecimalPoint = do before <- (many1 (oneOf "0123456789"))
                      string "."
                      after <- (many1 (oneOf "0123456789"))
                      return $ before ++ "." ++ after

tNumInteger :: Parser String
tNumInteger = many1 digit

tVarReference :: Parser String
tVarReference = do 

pDecimal :: Parser Double
pDecimal = do chars <- (try tNumDecimalPoint) <|> tNumInteger
              return $ read chars





