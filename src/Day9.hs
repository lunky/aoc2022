module Day9
    (
     day9,day9b,
     left,upLeft,up,upRight,right,downRight,down,downLeft,
     Coord(..)
    )
    where

import Data.List (foldl', nub)
    
day9 :: String -> Int 
day9 input = length $ nub $ tailPath $ headPath $ parseInput input

day9b :: String -> Int
day9b input = length $ nub (iterate tailPath (headPath $ parseInput input) !! 9)

data Coord = C !Int !Int
  deriving (Read, Show, Ord, Eq)

tailPath :: [Coord] -> [Coord]
tailPath = scanl tailPath' (C 0 0)

tailPath' from@(C fromY fromX) to@(C toY toX) 
  | abs (fromX - toX) <= 1 && abs (fromY - toY) <= 1 = from  -- head move of 0 or 1, no change
  | toX - fromX == -1 && toY - fromY == 2  = downLeft from    {-- 1  --}
  | toX == fromX && toY - fromY == 2       = down from        {-- 2  --}
  | toX - fromX == 1 && toY - fromY == 2   = downRight from   {-- 3  --}
  | toX - fromX == 2 && toY - fromY == 1   = downRight from   {-- 4  --}
  | toX - fromX == 2 && toY == fromY       = right  from      {-- 5  --}
  | toX - fromX == 2 && toY - fromY == -1  = upRight from     {-- 6  --}
  | toX - fromX == 1 && toY - fromY == -2  = upRight from     {-- 7  --}
  | toX == fromX && toY - fromY == - 2     = up from          {-- 8  --}
  | toX - fromX == -1 && toY - fromY == -2 = upLeft from      {-- 9  --}
  | toX - fromX == -2 && toY - fromY == -1 = upLeft from      {-- 10 --}
  | toX - fromX == -2 && toY - fromY == 0  = left from        {-- 11 --}
  | toX - fromX == -2 && toY - fromY == 1  = downLeft from    {-- 12 --}

  | toX - fromX == -2 && toY - fromY == 2  = downLeft from      {-- 13 --}
  | toX - fromX ==  2 && toY - fromY == 2  = downRight from     {-- 14 --}
  | toX - fromX ==  2 && toY - fromY == -2  = upRight from  {-- 15 --}
  | toX - fromX == -2 && toY - fromY == -2  = upLeft from   {-- 16 --}
  | otherwise = error "invalid movement"

headPath :: [(Direction,Distance)] -> [Coord]
headPath = reverse . foldl' travel [C 0 0]

travel (x:xs) (direction, distance) 
  | distance == 0 = x:xs
  | otherwise = case direction of 
                   GoUp    -> travel (up x   :(x:xs)) (direction, distance -1) 
                   GoDown  -> travel (down x :(x:xs)) (direction, distance -1) 
                   GoLeft  -> travel (left x :(x:xs)) (direction, distance -1) 
                   GoRight -> travel (right x:(x:xs)) (direction, distance -1) 

up:: Coord -> Coord
up (C y x) = C (y-1) x

upLeft :: Coord -> Coord
upLeft (C y x) = C (y-1) (x-1)

left :: Coord -> Coord
left  (C y x) = C y (x-1)

downLeft :: Coord -> Coord
downLeft  (C y x) = C (y+1) (x-1)

down :: Coord -> Coord
down (C y x) = C (y+1) x

right :: Coord -> Coord
right (C y x) = C y (x+1)

upRight :: Coord -> Coord
upRight (C y x) = C (y-1) (x+1)



downRight:: Coord -> Coord
downRight(C y x) = C (y+1) (x+1)


type Distance = Int
data Direction = GoUp | GoDown | GoLeft | GoRight deriving Show

class GoDirection a where
  go :: a -> Direction

instance GoDirection Char where
  go 'U' = GoUp
  go 'D' = GoDown
  go 'L' = GoLeft
  go 'R' = GoRight
  go a = error (show a)

parseInput :: String -> [(Direction, Distance)]
parseInput input = map (\(a:(_:b))->(go a,read b)) $ lines input
_input="R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2"
_input2="R 5\nU 8\nL 8\nD 3\nR 17\nD 10\nL 25\nU 20"
