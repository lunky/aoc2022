module Day18
    ( 
    day18
   ,day18b
   ,_input
    )
    where
    

import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as Set

data Point = P Int Int Int deriving (Show,Ord,Eq)

day18 :: String -> Int 
day18 input = sum $ map ((6 -) . sidesFound s) plainInput
  where 
    s = Set.fromList plainInput
    plainInput = parseInput input


day18b :: String -> Int
day18b input = 0

-- day18b' input = length $ nub $ filter (\y -> sidesFound s == 6) $ Set.
--   where 
--     partA = sum $ map ((6 -) . sidesFound s) plainInput
--     s = Set.fromList plainInput
--     plainInput = parseInput input
--     adjacentCubes = Set.fromList $ concatMap adjacent plainInput

parseInput :: String -> [Point]
parseInput input = map ((\[a,b,c]->P a b c). map read .splitOn ",") $ lines  input

sidesFound s p = length $ filter id $ map (`Set.member` s) $ adjacent p

adjacent (P x y z) = [P (x+1) y z,
                      P (x-1) y z,
                      P x (y+1) z,
                      P x (y-1) z,
                      P x y (z-1),
                      P x y (z+1)]

_input="2,2,2\n1,2,2\n3,2,2\n2,1,2\n2,3,2\n2,2,1\n2,2,3\n2,2,4\n2,2,6\n1,2,5\n3,2,5\n2,1,5\n2,3,5"
