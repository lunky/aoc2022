module Day6
    ( 
    day6
   ,day6b
   ,_input
    )
    where
import Data.List
    
day6 :: String -> Int 
day6 = day6base 4

day6b :: String -> Int
day6b = day6base 14

day6base :: Int -> String -> Int 
day6base offset input = day6base' (zip input [0..])
  where day6base' (x:xs) 
         | length (nub $ map fst $ take offset (x:xs)) == offset = snd $ (x:xs) !! offset
         | otherwise = day6base' xs

_input="mjqjpqmgbljsphdztnvjfqwrcgsmlb"
