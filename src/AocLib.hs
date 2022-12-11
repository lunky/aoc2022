module AocLib
    (   grid
      , adjacent
      , countBy
    ) where
import Data.List (foldl')

grid :: [[a]] -> [((Int,Int), a)]
grid = concat . zipWith (curry grid') [1..]
  where
    grid' (y,xs) = map (\(x,datum)-> ((x,y),datum)) $ zip [1..] xs

adjacent :: (Int,Int) -> [(Int,Int)]
adjacent (x,y) = [(x,y-1),(x,y+1),(x-1,y),(x+1,y)]

countBy :: Foldable f => (a -> Bool) -> f a -> Int
countBy p = foldl' (\acc x -> if p x then acc+1 else acc) 0
