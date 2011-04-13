
module JPath where

import Text.ParserCombinators.Parsec


-- Expression definition. Since, right now, everything in a JPath query is an
-- expression, we only need one datatype to hold all of this.
-- I've ordered the following list in order from highest precedence (binds
-- tightest) to lowest precedence, except where it doesn't make sense. Each
-- of the underscores toward the end of each line denotes the divisions
-- between operators of the same precedence.
--                                                                       ____
data Expr = LiteralNumber Double              -- 123, 123.45
          | LiteralString String              -- "hello world"
          | LiteralBoolean Bool               -- true, false
          | LiteralNull                       -- null
          | EmptyCollection                   -- ()                      ____
          | VarReference String               -- $x                      ____
          | NormalPattern String              -- x
          | PairPattern String                -- @x
          | Indexer Expr                      -- #x
          | PairIndexer Expr                  -- @#x                     ____
          | Path Expr Expr                    -- x/y
          | Predicate Expr Expr               -- x[y]                    ____
          | Multiply Expr Expr                -- x × y, x mul y, etc.
          | Divide Expr Expr                  -- x ÷ y, x div y, etc.    ____
          | Add Expr Expr                     -- x + y
          | Subtract Expr Expr                -- x - y                   ____
          | Otherwise Expr Expr               -- x otherwise y           ____
          | Equality Expr Expr                -- x = y
          | Inequality Expr Expr              -- x != y
          | GreaterThan Expr Expr             -- x > y
          | LessThan Expr Expr                -- x < y
          | GreaterOrEqual Expr Expr          -- x >= y
          | LessOrEqual Expr Expr             -- x <= y                  ____
          | AndTest Expr Expr                 -- x and y
          | OrTest Expr Expr                  -- x or y                  ____
          | PairConstructor Expr Expr         -- x:y                     ____
          | CollectionConstructor Expr Expr   -- x, y                    ____
          | ParenExpr Expr                    -- (x)
          | MapConstructor Expr               -- {x}
          | ListConstructor Expr              -- [x]                     ____
            deriving (Read, Show)


-- START GRAMMAR

-- tSomething = parser that generates literal output
-- pSomething = parser that generates Expr values
-- ppSomething = precedence parser that generates Expr values by oring a bunch
--               of other values
-- oSomething  = parser that generates oprator functions for use with chain1

-- Parses numbers with fractional components, like 1.5
tFractionalNumber :: Parser Double
tFractionalNumber = do before <- many1 digit
                       string "."
                       after <- many1 digit
                       return $ read $ before ++ "." ++ after

-- Parses whole numbers
tInteger :: Parser Double
tInteger = do chars <- many1 digit
              return $ read chars

-- Parses fractional numbers or whole numbers into LiteralNumber instances
pLiteralNumber :: Parser Expr
pLiteralNumber = do number <- (try tFractionalNumber) <|> tInteger
                    return $ LiteralNumber number

-- Parses a "\c" sequence, where c is a special char like n or r or some such
tStringEscape :: Parser Char
tStringEscape = do string "\\"
                   value <- noneOf "\r\n"
                   return $ case value of
                                 'n'  -> '\n'
                                 'r'  -> '\r'
                                 '\"' -> '\"'
                                 '\\' -> '\\'
                                 _    -> value

-- Parses an open quote, a series of chars or escapes (as per tStringEscape),
-- and a close quote, and returns a LiteralString representing the result
pLiteralString :: Parser Expr
pLiteralString = do string "\""
                    value <- many ((try $ noneOf "\"\r\n\\") <|> tStringEscape)
                    string "\""
                    return $ LiteralString value

-- parses "true" or "false" into a LiteralBoolean
pLiteralBoolean :: Parser Expr
pLiteralBoolean = do value <- (string "true") <|> (string "false")
                     return $ LiteralBoolean (value == "true")

-- parses "null" into LiteralNull
pLiteralNull :: Parser Expr
pLiteralNull = do string "null"
                  return LiteralNull

-- parses "()" into EmptyCollection
pEmptyCollection :: Parser Expr
pEmptyCollection = do string "()"
                      return EmptyCollection

-- parses a literal value
ppLiteralValue :: Parser Expr
ppLiteralValue = pLiteralBoolean <|> pLiteralNull <|> pEmptyCollection
                    <|> pLiteralString <|> pLiteralNumber  

pVarReference :: Parser Expr
pVarReference = do string "$"
                   value1 <- letter
                   value2 <- many1 alphaNum
                   return $ VarReference (value1:value2)

ppConcreteValue :: Parser Expr
ppConcreteValue = pVarReference <|> ppLiteralValue

pNormalPattern :: Parser Expr
pNormalPattern = do value1 <- letter
                    value2 <- many1 alphaNum
                    return $ NormalPattern (value1:value2)

pPairPattern :: Parser Expr
pPairPattern = do string "@"
                  value1 <- letter
                  value2 <- many1 alphaNum
                  return $ PairPattern (value1:value2)

pIndexer :: Parser Expr
pIndexer = do string "#"
              value <- pppExpr
              return $ Indexer value

pPairIndexer :: Parser Expr
pPairIndexer = do string "@#"
                  value <- pppExpr
                  return $ PairIndexer value
-- TODO: fix a problem where a patterm starting with "true" is getting parsed only partially and treated as LiteralBoolean True
ppMatchedValue :: Parser Expr
ppMatchedValue = (try ppConcreteValue) <|> (try pNormalPattern) <|>
                 (try pPairPattern) <|> (try pPairIndexer) <|> pIndexer

--- And finally, the result
pppExpr :: Parser Expr
pppExpr = ppMatchedValue

-- END GRAMMAR









































