
module Landfill.BF where

import qualified Data.Sequence as S

type Pointer = Int
type Cell = Int
type Memory = S.Seq Cell
type BFState = (Pointer, Memory)

type BF = State BFState

data AST = Up | Down | Right | Left | In | Out | Loop [AST]

getOneChar :: State String (Maybe Char)
getOneChar = do
    xs <- get
    case xs of
        [] -> return Nothing
        y:ys = do
            set ys
            return $ Just y

parse :: State String [AST]
parse = do
    mx <- getOneChar
    case mx of
        Nothing  -> return []
        Just "[" -> liftM Loop parse
        Just "]" -> return []
        Just x -> liftM (parse' x :) parse

parse' :: String -> AST
parse' '+' = Up
parse' '-' = Down
parse' '>' = Right
parse' '<' = Left
parse' ',' = In
parse' '.' = Out
 

run :: [AST] -> BF ()
run x:xs = 

adjustCurrentCell :: (Cell -> Cell) -> BF ()
adjustCurrentCell f = do
    pointer <- getPointer
    adjustMemory $ S.adjust f pointer

adjustMemory :: (Memory -> Memory) -> BF ()
adjustMemory f = do
    memory <- getMemory
    setMemory $ f memory

getCurrentCell :: BF Cell
getCurrentCell = do
    memory <- getMemory
    pointer <- getPointer
    return $ S.index memory pointer

setCurrentCell :: Cell -> BF ()
setCurrentCell value = do
    memory <- getMemory
    setMemory $ S.update pointer value memory

getMemory :: BF Memory
getMemory = BF $ \s -> (snd s, s)

getPointer :: BF Pointer
getPointer = BF $ \s -> (fst s, s)

setMemory :: Memory -> BF ()
setMemory m = BF $ \(p, _) -> ((), (p, m))

setPointer :: BF Pointer
setPointer p  BF $ \(_, m) -> ((), (p, m)) 









