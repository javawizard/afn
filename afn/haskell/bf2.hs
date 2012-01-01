
import qualified Data.Map as Map
import Data.Maybe (isJust, fromJust)
import Data.IORef

data Instruction = Read | Write | Forward | Back | Up | Down | Loop Program
    deriving (Show, Eq)
data Program = Sequence Instruction Program | End
    deriving (Show, Eq)
type Pointer = Int
type IOPointer = IORef Pointer
type Cell = Int

class Data a where
    increment :: a -> Pointer -> IO ()
    decrement :: a -> Pointer -> IO ()
    getData :: a -> Pointer -> IO Cell
    setData :: a -> Pointer -> Cell -> IO ()

instance Data (IORef [Cell]) where
    -- TODO: implement this

instructionMap = Map.fromList [(',', Read), ('.', Write), ('>', Forward), 
                               ('<', Back), ('+', Up), ('-', Down)]

cellToChar :: Cell -> Char
cellToChar = toEnum
charToCell :: Char -> Cell
charToCell = fromEnum

getInstruction :: Char -> Maybe Instruction
getInstruction = flip Map.lookup instructionMap -- getInstruction ">" = Forward

parse :: String -> Program
parse code = case parse_ code of
    (result, []) -> result
    (_, _)       -> error "Too many ']'"

parse_ :: String -> (Program, String) -- Return value is (program, leftover text)
parse_ (']':xs) = (End, xs)
parse_ "" = (End, "")
parse_ ('[':xs) = (Sequence (Loop loopcode) subprogram, remainder) where
    (loopcode, loopremainder) = parse_ xs
    (subprogram, remainder) = parse_ loopremainder
parse_ (x:xs) | isJust $ getInstruction x = (Sequence (fromJust $ getInstruction x) subprogram, remainder) where
    (subprogram, remainder) = parse_ xs
parse_ x = error $ "Invalid char: " ++ x

run :: Program -> IOPointer -> Data -> IO ()
run End ioPointer dataBank = return ()
run (Sequence instruction program) ioPointer dataStorage = do
    pointer <- readIORef ioPointer
    runInstruction instruction pointer ioPointer dataStorage

runInstruction :: Instruction -> Pointer -> IOPointer -> Data -> IO ()
runInstruction Up pointer _ dataStorage = increment dataStorage pointer
runInstruction Down pointer _ dataStorage = decrement dataStorage pointer
runInstruction Forward _ ioPointer _ = modifyIORef ioPointer ((+) 1)
runInstruction Back _ ioPointer _ = modifyIORef ioPointer ((-) 1)
runInstruction Write pointer _ dataStorage = do
    d <- getData dataStorage pointer
    putChar $ cellToChar d
runInstruction Read pointer _ dataStorage = do
    c <- getChar
    setData dataStorage pointer $ charToCell c
runInstruction (Loop program) pointer ioPointer dataStorage = do
    d <- getData dataStorage pointer
    if d /= 0
        then do
            run program ioPointer dataStorage
            runInstruction (Loop program) pointer ioPointer dataStorage
        else
            return ()























