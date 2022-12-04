module Day4
    ( 
    day4
   ,day4b
   ,_input
    )
    where
import Data.List.Split
    
day4 :: String -> Int 
day4 input = length $ filter (==True) $ map ((\[a,b] -> fullOverlap a b) . map range ) (parseInput input)

day4b :: String -> Int
day4b input = length $ filter (==True) $ map ((\[a,b] -> anyOverlap a b) . map range ) (parseInput input)

range [from,to] = [from..to]

parseInput :: String -> [[[Int]]]
parseInput input = map (map (map read . splitOn "-") . splitOn ",") $ lines input

intersect :: Eq a => [a] -> [a] -> [a]
intersect [] = const []
intersect xs = filter (`elem` xs)

fullOverlap xs ys = intersection == xs || intersection == ys
  where 
    intersection = xs `intersect` ys

anyOverlap xs ys = not (null intersection)
  where 
    intersection = xs `intersect` ys

_input="2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8"
