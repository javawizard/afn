
module JPath where

import Text.ParserCombinators.Parsec
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List

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
          | ParenExpr Expr                    -- (x)
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

-- parses "()" into EmptyCollection
pEmptyCollection :: Parser Expr
pEmptyCollection = do string "()"
                      return EmptyCollection

pVarReference :: Parser Expr
pVarReference = do string "$"
                   value1 <- letter
                   value2 <- many1 alphaNum
                   return $ VarReference (value1:value2)

pParenExpr :: Parser Expr
pParenExpr = do string "("
                expr <- pppExpr
                string ")"
                return $ ParenExpr expr

ppSingleValue :: Parser Expr
ppSingleValue = pVarReference <|> (try pEmptyCollection) <|> pParenExpr <|> 
                  pLiteralString <|> pLiteralNumber

------------------------------------------------------------------------------

-- Parses normal patterns. This also parses "true", "false", and "null" since
-- they're essentially just special patterns.

pNormalPattern :: Parser Expr
pNormalPattern = do value1 <- letter
                    value2 <- many alphaNum
                    return $ case (value1:value2) of
                                  "true"  -> LiteralBoolean True
                                  "false" -> LiteralBoolean False
                                  "null"  -> LiteralNull
                                  x       -> NormalPattern x 

pPairPattern :: Parser Expr
pPairPattern = do string "@"
                  value1 <- letter
                  value2 <- many alphaNum
                  return $ PairPattern (value1:value2)

pIndexer :: Parser Expr
pIndexer = do string "#"
              value <- pppExpr
              return $ Indexer value

pPairIndexer :: Parser Expr
pPairIndexer = do string "@#"
                  value <- pppExpr
                  return $ PairIndexer value

ppMatchedValue :: Parser Expr
ppMatchedValue = (try ppSingleValue) <|> (try pNormalPattern) <|>
                 (try pPairPattern) <|> (try pPairIndexer) <|> pIndexer

------------------------------------------------------------------------------

data PathComponentOrPredicate = PathComponent Expr | PredicateComponent Expr

tPathComponent :: Parser PathComponentOrPredicate
tPathComponent = do string "/"
                    expr <- ppMatchedValue
                    return $ PathComponent expr

tPredicate :: Parser PathComponentOrPredicate
tPredicate = do string "["
                expr <- pppExpr
                string "]"
                return $ PredicateComponent expr

tPathComponentOrPredicate :: Parser PathComponentOrPredicate
tPathComponentOrPredicate = tPathComponent <|> tPredicate

pPathSeries :: Parser Expr
pPathSeries = do initial <- ppMatchedValue
                 components <- many (tPredicate <|> tPathComponent)
                 return $ foldl (\x y -> case y of
                                              PathComponent e      -> Path x e
                                              PredicateComponent e -> Predicate x e
                                ) initial components 

ppPathValue :: Parser Expr
ppPathValue = pPathSeries

------------------------------------------------------------------------------

sLastValue2 = ppPathValue

tCollectionConstructorSuffix :: Parser Expr
tCollectionConstructorSuffix = do string ","
                                  value <- sLastValue2
                                  return value

pCollectionConstructor = do initial <- sLastValue2
                            components <- many tCollectionConstructorSuffix
                            return $ foldl CollectionConstructor initial components

ppCollectionConstructor = pCollectionConstructor

------------------------------------------------------------------------------

sLastValue1 = pCollectionConstructor

pMapConstructor :: Parser Expr
pMapConstructor = do string "{"
                     value <- sLastValue1
                     string "}"
                     return $ MapConstructor value

pListConstructor :: Parser Expr
pListConstructor = do string "["
                      value <- sLastValue1
                      string "]"
                      return $ ListConstructor value

pMapOrListOrOther :: Parser Expr
pMapOrListOrOther = (try pMapConstructor) <|> (try pListConstructor) <|> sLastValue1

------------------------------------------------------------------------------

--- And finally, the result
pppExpr :: Parser Expr
pppExpr = pMapOrListOrOther

------------------------------------------------------------------------------
--                              END GRAMMAR                                 --
------------------------------------------------------------------------------

-- Now the interpreter.

-- context item, vars
data Context = Context Item (Map String Collection)

-- Not using this for now.    data Result = Result Collection

-- JSONObject: number of pairs, lookup pair by index, has pair by key, lookup pair by key
-- JSONList: number of items, lookup item by index
data Item = JSONObject   Int (Int -> Item) (String -> Bool) (String -> Item)
          | JSONList     Int (Int -> Item)
          | JSONString   String
          | JSONNumber   Double
          | JSONBoolean  Bool
          | JSONNull
          | Pair         String Item


type Collection = [Item]

-- Gets all items in the specified list or object given the number of items
-- and the indexer function
getAllItems :: Int -> (Int -> Item) -> [Item]
getAllItems total indexer = map indexer [0 .. (total - 1)]

outputJSON :: Item -> String
outputJSON (JSONObject total indexer _ _) = "{" ++ (intercalate ", " $ map outputJSON $ getAllItems total indexer) ++ "}"
outputJSON (JSONList total indexer) = "[" ++ (intercalate ", " $ map outputJSON $ getAllItems total indexer) ++ "]"
outputJSON (JSONString string) = show string
outputJSON (JSONNumber number) = show number
outputJSON (JSONBoolean boolean) = case boolean of {True -> "true"; False -> "false"}
outputJSON JSONNull = "null"
outputJSON (Pair k v) = (show k) ++ ": " ++ (outputJSON v)

outputResultData :: Collection -> String
outputResultData items = intercalate "\n" $ map outputJSON items

defaultContext :: Context
defaultContext = Context JSONNull Map.empty

parseQuery :: String -> Either ParseError Expr
parseQuery text = parse pppExpr "" text

evaluateQuery :: Context -> String -> Either ParseError Collection
evaluateQuery context text = case parseQuery text of
                                  (Left  e) -> Left e
                                  (Right v) -> Right $ evaluate context v

evaluateQueryWithResult :: Context -> String -> String
evaluateQueryWithResult context text = case evaluateQuery context text of
                                            (Left  e) -> error $ show e
                                            (Right r) -> outputResultData r

evaluate :: Context -> Expr -> Collection



evaluate _ (LiteralNumber number) = [JSONNumber  number]
evaluate _ (LiteralString string) = [JSONString  string]
evaluate _ (LiteralBoolean bool)  = [JSONBoolean bool]
evaluate _ LiteralNull            = [JSONNull]
evaluate c (ParenExpr expr)       = evaluate c expr
evaluate _ EmptyCollection        = []

evaluate (Context _ vars) (VarReference name) = vars Map.! name

evaluate (Context (JSONObject _ _ hasKey lookup) _) (NormalPattern p) = if hasKey p then [extractPairValue $ lookup p] else []
evaluate (Context (Pair k v) _)                     (NormalPattern "key") = [JSONString k]
evaluate (Context (Pair k v) _)                     (NormalPattern "value") = [v]
evaluate (Context _ _)                              (NormalPattern _) = []

evaluate (Context (JSONObject _ _ hasKey lookup) _) (PairPattern p) = if hasKey p then [lookup p] else []
evaluate (Context _ _)                              (PairPattern _) = []

evaluate c@(Context (JSONObject _ _ hasKey lookup) _) (Indexer v) = let p = (extractString $ evaluate c v !! 0) in if hasKey p then [extractPairValue $ lookup p] else []
evaluate c@(Context (Pair key value) _)               (Indexer v) = case extractString $ evaluate c v !! 0 of {"key" -> [JSONString key]; "value" -> [value]}
evaluate c@(Context (JSONList total indexer) _)       (Indexer v) = let p = (extractInt $ evaluate c v !! 0) - 1 in if p >= 0 && p < total then [indexer p] else []
evaluate (Context _ _)                                (Indexer _) = []

evaluate c@(Context i vars) (Path l r) = concat [evaluate (Context nci vars) r | nci <- evaluate c l]

evaluate c (CollectionConstructor l r) = (evaluate c l) ++ (evaluate c r)

evaluate c (ListConstructor e) = [buildListFromHaskellList $ evaluate c e]

------------------------------------------------------------------------------

extractPairKey :: Item -> String
extractPairKey (Pair k v) = k

extractPairValue :: Item -> Item
extractPairValue (Pair k v) = v

extractString :: Item -> String
extractString (JSONString string) = string

extractInt :: Item -> Int
extractInt (JSONNumber number) = floor number 

buildObjectFromList :: [(String, Item)] -> Item
buildObjectFromList v = buildObjectFromMap $ Map.fromList v

buildObjectFromMap :: Map String Item -> Item
buildObjectFromMap m = JSONObject (Map.size m)
                                  (\i -> case (Map.elemAt i m) of {(k,v) -> Pair k v})
                                  (\k -> Map.member k m)
                                  (\k -> Pair k (m Map.! k))

buildListFromHaskellList :: [Item] -> Item
buildListFromHaskellList l = JSONList (length l) ((!!) l) 



































