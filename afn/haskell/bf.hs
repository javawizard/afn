
-- set x:xs i v = x:(set xs i-1 v)
-- set x:xs 0 v = v:xs

-- type Storage = [Cell]
-- type Cell = Int

{-
Ok, let's see... So...

We hand the interpreter an instruction to run, a storage bank, and the rest of
the code to run after running that instruction.

If we hand it no instruction to run, it does nothing and returns the storage
bank unmodified.

If we hand it the end of a loop, it does nothing and returns the storage bank
unmodified.

If we hand it any of the six core instructions, it performs that instruction,
then passes the new storage bank to the interpreter again starting with the
next instruction to run.

If we hand it a loop, it checks to see if the current cell is nonzero. If it is,
it invokes the interpreter on the first instruction after the start loop, then
invokes the interpreter again on the loop itself but passing in the storage bank
returned from invoking the instruction after the start of the loop.
-}

data AST = R | W | F | B | I | D | S | E -- Read Write Forward Back Increment Decrement StartLoop EndLoop
data Instruction = Read | Write | Forward | Back | Increment | Decrement | Loop Program
type Program = [Instruction]

type Cell = Int
type Storage = [IORef Cell]

setCell :: Storage -> Int -> Cell -> IO ()
setCell storage i v = writeIORef (storage !! i) v

getCell :: Storage -> Int -> IO Cell
getCell storage i = readIORef $ storage !! i

modifyCell :: Storage -> Int -> (Cell -> Cell) -> IO ()
modifyCell storage i f = modifyIORef (storage !! i) f

parse :: String -> [AST]
parse xs = map parseChar xs

parseChar "," = R
parseChar "." = W
parseChar ">" = F
parseChar "<" = B
parseChar "+" = I
parseChar "-" = D
parseChar "[" = S
parseChar "]" = E

compile :: [AST] -> Program

compileSection :: [AST] -> (Program, [AST])
compileSection [] = ([], [])
compileSection (E:xs) = ([], xs)
compileSection (S:xs) = (Loop ):
compileSection (x:xs) = let (after:remainder) = compileSection xs in
    let v = case x of
        R -> Read
        W -> Write
        F -> Forward
        B -> Backward
        I -> Increment
        D -> Decrement
    in (v:after, remainder)

run :: Program -> Storage -> IORef Int -> IO ()
run program@(instruction:next) storage pointerRef = do
    -- Get the current storage pointer
    pointer <- readIORef pointerRef
    -- Now figure out what to do for this instruction
    case instruction of
        Read -> setCell storage pointer $ ord getChar
        Write -> do {c <- getCell storage pointer; putChar c}
        Forward -> modifyIORef pointerRef ((+) 1)
        Back -> modifyIORef pointerRef ((-) 1)
        Increment -> modifyCell storage pointer ((+) 1)
        Decrement -> modifyCell storage pointer ((-) 1)
        (Loop contents) -> do
            -- Get the value of the current cell
            value <- getCell storage pointer
            if value != 0
                then do
                    -- If it's nonzero, run the loop's body, then run the loop
                    -- itself again
                    run contents storage pointerRef
                    run program storage pointerRef
                else return ()
    run next storage pointerRef
run [] storage pointerRef = return ()

            











































