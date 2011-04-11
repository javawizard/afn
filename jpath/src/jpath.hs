
module JPath where

import Text.ParserCombinators.Parsec


-- Expression definition. Since, right now, everything in a JPath query is an
-- expression, we only need one datatype to hold all of this.

data Expr = LiteralNumber Double              -- 123, 123.45
          | LiteralString String              -- "hello world"
          | LiteralBoolean Bool               -- true, false
          | LiteralNull                       -- null
          | Add Expr Expr                     -- x + y
          | Subtract Expr Expr                -- x - y
          | Multiply Expr Expr                -- x × y, x mul y, etc.
          | Divide Expr Expr                  -- x ÷ y, x div y, etc.
          | Equality Expr Expr                -- x = y
          | Inequality Expr Expr              -- x != y
          | GreaterThan Expr Expr             -- x > y
          | LessThan Expr Expr                -- x < y
          | GreaterOrEqual Expr Expr          -- x >= y
          | LessOrEqual Expr Expr             -- x <= y
          | Predicate Expr Expr               -- x[y]
          | Path Expr Expr                    -- x/y
          | VarReference String               -- $x
          | PairConstructor Expr Expr         -- x:y
          | MapConstructor Expr               -- {x}
          | ListConstructor Expr              -- [x]
          | CollectionConstructor Expr Expr   -- x, y
          | NormalPattern String              -- x
          | PairPattern String                -- @x
          | Indexer Expr                      -- #x
          | PairIndexer Expr                  -- @#x


-- START GRAMMAR

pLiteralFractionalNumber :: Parser Double
pLiteralFractionalNumber = do before <- many1 digit
                              string "."
                              after <- many1 digit
                              return $ read $ before ++ "." ++ after

pLiteralInteger :: Parser Double
pLiteralInteger = do chars <- many1 digit
                     return $ read chars

pLiteralNumber :: Parser Double
pLiteralNumber = (try pLiteralFractionalNumber) <|> pLiteralInteger

-- END GRAMMAR
