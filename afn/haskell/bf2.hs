
import qualified Data.Map (fromList, lookup)

data Instruction = Read | Write | Forward | Back | Up | Down | Loop Program
    deriving (Show, Eq)
data Program = Sequence Instruction Program | End
    deriving (Show, Eq)

instructionMap = Map.fromList [(',', Read), ('.', Write), ('>', Forward), 
                               ('<', Back), ('+', Up), ('-', Down)]

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
parse_ ('>':xs) = (Sequence Forward subprogram, remainder) where (subprogram, remainder) = parse_ xs
parse_ x = error $ "Invalid char: " ++ x

