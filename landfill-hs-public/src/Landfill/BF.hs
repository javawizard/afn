
module Landfill.BF where

import qualified Data.Sequence as S

type Pointer = Int
type Cell = Int
type Memory = S.Seq Cell
type BFState = (Pointer, Memory)

data BF a = BF (BFState -> (a, BFState))

extractBF :: BF a -> BFState -> (a, BFState)
extractBF (BF f) = f

instance Monad BF where
    m >>= f = BF $ \state -> let (a, newState) = extractBF m s in extractBF (f a) newState
    return a = BF $ \s -> (a, s)

data AST = Up | Down | Right | Left | In | Out | Loop [AST]

parse :: State s 

parse' :: String -> AS
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









