module Day10
    ( 
    day10
   ,day10b
   ,_input
    )
    where
import Data.List.Split

data Instruction = NoOp | AddX !Int
  deriving (Read, Show, Ord, Eq)

type Delay = Int

data DelayedInstruction = DelayedInstruction Delay Int deriving (Read, Show, Ord, Eq)
    
day10 :: String -> Int 
day10 input = sum [i * instructions!!(i-1) | i <- [20,60,100,140,180,220]]
  where instructions = day10' $ parseInput input


isNear :: Int -> Int -> Bool
isNear x y = abs (x - y) <= 1

crt :: [Int] -> String
crt xs = unlines [ [if near then '#' else ' ' | near <- zipWith isNear [0..] row]
                 | row <- take 6 (chunksOf 40 xs)]


day10b :: String -> String
day10b input = crt instructions
  where instructions = day10' $ parseInput input

parseInput :: String -> [Instruction]
parseInput = map parseInstruction . lines
  where 
    parseInstruction :: String -> Instruction
    parseInstruction "noop" = NoOp
    parseInstruction input = AddX $ read $ drop 5 input

_input="addx 15\naddx -11\naddx 6\naddx -3\naddx 5\naddx -1\naddx -8\naddx 13\naddx 4\nnoop\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx -35\naddx 1\naddx 24\naddx -19\naddx 1\naddx 16\naddx -11\nnoop\nnoop\naddx 21\naddx -15\nnoop\nnoop\naddx -3\naddx 9\naddx 1\naddx -3\naddx 8\naddx 1\naddx 5\nnoop\nnoop\nnoop\nnoop\nnoop\naddx -36\nnoop\naddx 1\naddx 7\nnoop\nnoop\nnoop\naddx 2\naddx 6\nnoop\nnoop\nnoop\nnoop\nnoop\naddx 1\nnoop\nnoop\naddx 7\naddx 1\nnoop\naddx -13\naddx 13\naddx 7\nnoop\naddx 1\naddx -33\nnoop\nnoop\nnoop\naddx 2\nnoop\nnoop\nnoop\naddx 8\nnoop\naddx -1\naddx 2\naddx 1\nnoop\naddx 17\naddx -9\naddx 1\naddx 1\naddx -3\naddx 11\nnoop\nnoop\naddx 1\nnoop\naddx 1\nnoop\nnoop\naddx -13\naddx -19\naddx 1\naddx 3\naddx 26\naddx -30\naddx 12\naddx -1\naddx 3\naddx 1\nnoop\nnoop\nnoop\naddx -9\naddx 18\naddx 1\naddx 2\nnoop\nnoop\naddx 9\nnoop\nnoop\nnoop\naddx -1\naddx 2\naddx -37\naddx 1\naddx 3\nnoop\naddx 15\naddx -21\naddx 22\naddx -6\naddx 1\nnoop\naddx 2\naddx 1\nnoop\naddx -10\nnoop\nnoop\naddx 20\naddx 1\naddx 2\naddx 2\naddx -6\naddx -11\nnoop\nnoop\nnoop"

day10' :: [Instruction] -> [Int]
day10' = run 1
  where
    run x [] = [x]
    run x (NoOp : xs) = x : run x xs
    run x (AddX y  : xs) = x : x : run (y+x) xs
