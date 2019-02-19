import System.Environment
import System.IO
import Data.Maybe
import Data.Char 

--  Date Type for Brainfuck operators
data Command = Rgt -- >
  | Lft   -- <
  | Inc   -- +
  | Dec   -- -
  | Jmp   -- [
  | End   -- ]
  | Rd    -- ,
  | Wrt   -- .
    deriving (Eq, Show)

displayBf :: Program -> String
displayBf = map toBrainfuck
  where 
    toBrainfuck c = case c of
      Rgt -> '>'
      Lft -> '<'
      Inc -> '+'
      Dec -> '-'
      Jmp -> '['
      End -> ']'
      Rd  -> ','
      Wrt -> '.'

-- Type Alias for a Brainfuck program
type Program = [Command]

-- Spits out a list of readable tokens corresponding to Brainfuck operators
-- Also, removes unnecessary characters as comments
tokenize :: String -> Program
tokenize = mapMaybe toCommand
  where 
    toCommand c = case c of 
      '>' -> Just Rgt
      '<' -> Just Lft
      '+' -> Just Inc
      '-' -> Just Dec
      '[' -> Just Jmp
      ']' -> Just End
      ',' -> Just Rd
      '.' -> Just Wrt
      _   -> Nothing

-- Data Type for output tape - Left Pivot Right
data Tape a = Tape [a] a [a]

-- Generate empty tape
emptyTape :: Tape Int 
emptyTape = Tape zeroes 0 zeroes
  where zeroes = repeat 0

-- Functions for moving on the tape
moveRight :: Tape a -> Tape a
moveRight (Tape ls m (r:rs)) = Tape (m:ls) r rs

moveLeft :: Tape a -> Tape a
moveLeft (Tape (l:ls) m rs) = Tape ls l (m:rs)

-- Functions for interpreting 
repl :: IO ()
repl = 
  do
    line <- getLine
    interpret $ tokenize line
    repl

interpret :: Program -> IO ()
interpret = (runCommand emptyTape) . toTape
  where toTape (b:bs) = Tape [] b bs

-- Executes one command
runCommand :: Tape Int -> Tape Command -> IO ()

-- Move Instructions
runCommand dataTape source@(Tape _ Rgt _) = advance (moveRight dataTape) source
runCommand dataTape source@(Tape _ Lft _) = advance (moveLeft dataTape) source

-- Inc, Dec Instructions
runCommand (Tape l m r) source@(Tape _ Inc _) = advance (Tape l (m+1) r) source
runCommand (Tape l m r) source@(Tape _ Dec _) = advance (Tape l (m-1) r) source

-- Read, Write Instructions
runCommand dataTape@(Tape _ m _) source@(Tape _ Wrt _) = 
    do
      putChar (chr m)
      hFlush stdout
      advance dataTape source

runCommand dataTape@(Tape l _ r) source@(Tape _ Rd _) = 
    do
      p <- getChar
      advance (Tape l (ord p) r) source

-- Loop Instructions
runCommand dataTape@(Tape _ m _) source@(Tape _ Jmp _) 
  -- If element in cell is zero, move to corresponding End instruction
  | m == 0 = seekEnd 0 dataTape source
  -- Else proceed normally
  | otherwise = advance dataTape source

runCommand dataTape@(Tape _ m _) source@(Tape _ End _) 
  -- If element in cell is not zero, move to corresponding Jmp instruction
  | m /= 0 = seekJmp 0 dataTape source
  -- Else proceed normally
  | otherwise = advance dataTape source

-- The integer keeps track of the nesting of [,]. 
seekEnd :: Int -> Tape Int -> Tape Command -> IO ()
seekEnd 1 dataTape source@(Tape _ End _) = advance dataTape source
seekEnd b dataTape source@(Tape _ End _) = seekEnd (b-1) dataTape (moveRight source)
seekEnd b dataTape source@(Tape _ Jmp _) = seekEnd (b+1) dataTape (moveRight source)
seekEnd b dataTape source = seekEnd b dataTape (moveRight source)

seekJmp :: Int -> Tape Int -> Tape Command -> IO ()
seekJmp 1 dataTape source@(Tape _ Jmp _) = advance dataTape source
seekJmp b dataTape source@(Tape _ Jmp _) = seekJmp (b-1) dataTape (moveLeft source)
seekJmp b dataTape source@(Tape _ End _) = seekJmp (b+1) dataTape (moveLeft source)
seekJmp b dataTape source = seekJmp b dataTape (moveLeft source)

-- Move forward on source tape, and execute  
advance :: Tape Int -> Tape Command -> IO ()
advance dataTape (Tape _ _ []) = return ()
advance dataTape source = runCommand dataTape (moveRight source)

main :: IO ()
main = 
  do 
    args <- getArgs
    if null args
      then 
        do
          repl
      else 
        if null $ tail args
          then
            do
              let file = head args
              contents <- readFile file
              interpret $ tokenize contents
          else
            putStrLn "Usage : ./brainfuck [file-name]" 