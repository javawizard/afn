
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

run' :: AST -> BF ()
run Up = adjustCurrentCell (+ 1)
run Down = adjustCurrentCell (- 1)
run Left = adjustPointer (+ 1)
run Right = adjustPointer (- 1)

adjustCurrentCell :: (Cell -> Cell) -> BF ()
adjustCurrentCell f = do
    pointer <- getPointer
    adjustMemory $ S.adjust f pointer

adjustPointer :: (Pointer -> Pointer) -> BF ()
adjustPointer f = do
    pointer <- getPointer
    setPointer $ f pointer

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
getMemory = do
    (p, m) <- get
    return m

getPointer :: BF Pointer
getPointer = do
    (p, m) <- get
    return p

setMemory :: Memory -> BF ()
setMemory m = do
    (p, _) <- get
    set (p, m)

setPointer :: BF Pointer
setPointer p = do
    (_, m) <- get
    set (p, m) 









