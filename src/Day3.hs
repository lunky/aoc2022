module Day3
    ( 
    day3
   ,day3b
   ,_input
    )
    where

import Data.Maybe (fromJust)
import Data.List (elemIndex)
import Data.List.Split (chunksOf)
    
day3 :: String -> Int 
day3 input = sum $ map (\(a,b)-> rank $ head $ intersect a b) $ parseInput input

day3b :: String -> Int
day3b input = sum $ map (\[a,b,c] -> rank $ threeWayIntersect a b c) $ parseInputb input 


threeWayIntersect :: Eq a => [a] -> [a] -> [a] -> a
threeWayIntersect a b c = head $ intersect (a `intersect` b) (b `intersect` c)

intersect :: Eq a => [a] -> [a] -> [a]
intersect [] = const []
intersect xs = filter (`elem` xs)

rank :: Char -> Int
rank c = (+) 1 $ fromJust $ elemIndex c "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" 

parseInput :: String -> [(String,String)]
parseInput input = map (\y->splitAt (length y `div` 2) y) $ lines input

parseInputb :: String -> [[String]]
parseInputb input = chunksOf 3 $ lines input

_input="vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw"
