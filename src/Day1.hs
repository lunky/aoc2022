module Day1
    (   day1
      , day1b
    ) where
import Data.List
import Data.List.Split

day1 :: String -> Int
day1 input = maximum $ 
                map (sum . map (\ y -> read y :: Int) . lines) 
                (splitOn "\n\n" input)

day1b :: String -> Int
day1b input = sum $ take 3 $ reverse $ sort $
                map (sum . map (\ y -> read y :: Int) . lines) 
                (splitOn "\n\n" input)

_input = "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000\n";


