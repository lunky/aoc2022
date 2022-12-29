{-# Language FlexibleContexts, ImportQualifiedPost, UnboxedTuples, MagicHash, MultiParamTypeClasses, DeriveDataTypeable, DeriveGeneric, TypeFamilies, TypeOperators, BlockArguments #-}
module Day12
    ( 
    day12
   ,day12b
   ,_input
    )
    where
import Data.Char (digitToInt, ord)
import Data.List (unfoldr)
import Data.Array.Base 
import Data.Array.IArray
import Data.Map (Map)
import qualified Data.Map as M

data Coord = C !Int !Int
  deriving (Read, Show, Ord, Eq)

type Grid = UArray Coord Char 

above :: Coord -> Coord
above (C y x) = C (y-1) x

below :: Coord -> Coord
below (C y x) = C (y+1) x

left :: Coord -> Coord
left  (C y x) = C y (x-1)

right :: Coord -> Coord
right (C y x) = C y (x+1)

around :: Grid -> Coord -> [Coord]
around a c = filter inside $ map (\y-> y c) [above,below,left,right]
  where inside x = inRange (bounds a) x

valid :: Grid -> Coord -> [Coord]
valid a c = filter (\y-> y==end || greater a y c) $ around a c
  where end = findEnd a



findStart a = fst $ head $ filter (\(a,b)->b=='S') $ assocs a
findEnd a = fst $ head $ filter (\(a,b)->b=='E') $ assocs a

day12' a = unfoldr next (a,M.empty,([start],[]))
  where start = findStart a


next (a,m,(toProcess,parents)) =  if null toProcess
                then Nothing
                else Just ((c,parents), (a,M.insert c c m,(found ++ tail toProcess,c:parents))) 
  where found = filter (`M.notMember` m) $ valid a c
        c = head toProcess
        

greater :: Grid -> Coord -> Coord -> Bool
greater a c x = a!c == 'C'  || a!x == 'S' || (ord (a!c) - ord (a!x)) == 1
                                          || (ord (a!c) - ord (a!x)) == 0

instance Ix Coord where
  index (C lorow locol, C hirow hicol) (C row col) = index ((lorow,locol), (hirow,hicol)) (row,col)
  inRange (C lorow locol, C hirow hicol) (C row col) = inRange (lorow,hirow) row && inRange (locol,hicol) col
  range (C lorow locol, C hirow hicol) = [C row col | row <- [lorow..hirow], col <- [locol..hicol]]

parseInput :: String -> Grid 
parseInput input = listArray (C 0 0, C (length xs - 1) (length (head xs) - 1)) (concat xs)
  where xs = lines input
    
day12 :: String -> Int 
day12 input =  length (M.fromList (day12' a) M.! end) - 1
  where a = parseInput input
        end = findEnd a

day12b :: String -> Int
day12b input = 0
_input="Sabqponm\nabcryxxl\naccszExk\nacctuvwj\nabdefghi"

{--
  01234567
0 Sabqponm
1 abcryxxl
2 accszExk
3 acctuvwj
4 abdefghi
--}
