{-# Language FlexibleContexts #-}
module Day8
    ( 
    day8
   ,day8b
   ,_input
    )
    where

import Data.Array.Base 
import Data.Array.IArray
import Data.Char (digitToInt)
import AocLib

day8 :: String -> Int 
day8 input = countBy (isEdgeVisible arr) (range (bounds arr))
  where arr = parseInput input

day8b :: String -> Int
day8b input = maximum (map (scenicScore arr) (range (bounds arr)))
  where arr = parseInput input

instance Ix Coord where
  index (C lorow locol, C hirow hicol) (C row col) = index ((lorow,locol), (hirow,hicol)) (row,col)
  inRange (C lorow locol, C hirow hicol) (C row col) = inRange (lorow,hirow) row && inRange (locol,hicol) col
  range (C lorow locol, C hirow hicol) = [C row col | row <- [lorow..hirow], col <- [locol..hicol]]

parseInput input = listArray (C 0 0, C (length xs - 1) (length (head xs) - 1)) (concat xs)
  where xs = map (map digitToInt) $ lines input

data Coord = C !Int !Int
  deriving (Read, Show, Ord, Eq)

above :: Coord -> Coord
above (C y x) = C (y-1) x

below :: Coord -> Coord
below (C y x) = C (y+1) x

left :: Coord -> Coord
left  (C y x) = C y (x-1)

right :: Coord -> Coord
right (C y x) = C y (x+1)

outwards :: UArray Coord Int -> Coord -> (Coord -> Coord) -> [Int] 
outwards a start direction = [a ! i | i <- takeWhile (inRange (bounds a)) (iterate direction start)]

vectors :: UArray Coord Int -> Coord -> [[Int]]
vectors a c = map (outwards a c) [above,below,left,right]

isEdgeVisible :: UArray Coord Int -> Coord -> Bool
isEdgeVisible a c = any visible (vectors a c)

visible :: [Int] -> Bool
visible [] = error "empty list"
visible (x:xs) = all (<x) xs

scenicScore :: UArray Coord Int -> Coord -> Int
scenicScore a c = product (map treesSeen (vectors a c))

treesSeen :: [Int] -> Int
treesSeen [] = 0
treesSeen (x:xs) =
    case break (>= x) xs of
        (a,[])  -> length a
        (a,_:_) -> length a + 1

_input="30373\n25512\n65332\n33549\n35390"
