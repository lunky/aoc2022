{-# LANGUAGE TupleSections #-}
module Day14
    ( 
    day14
   ,day14b
   ,_input
    )
    where
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map

data Spot = Rock | Sand deriving (Show, Eq)
type Coord = (Int,Int)
type CaveMap = (Map Coord Spot, Int)
    
day14 :: String -> Int 
day14 input = Map.foldr countSand 0 $ fst $ fall (m,maxY)
  where m = parseInput input
        maxY = Map.foldrWithKey (\(_,y) _  acc-> if y > acc then y else acc) 0 m
        countSand x acc = if x == Sand then acc + 1 else acc


day14b :: String -> Int 
day14b input = Map.foldr countSand 0 $ fst $ fall (m,maxY+3)
  where m = Map.union intermediateM (Map.fromList $ zip (vector ((0,maxY+2),(1000,maxY+2))) $ repeat Rock)
        intermediateM = parseInput input
        maxY = Map.foldrWithKey (\(_,y) _  acc-> if y > acc then y else acc) 0 intermediateM
        countSand x acc = if x == Sand then acc + 1 else acc


parseInput :: String -> Map Coord Spot
parseInput input = Map.fromList $ 
                        map (,Rock) $ 
                        concat $ 
                        concatMap ((map vector . (\y -> zip y (tail y)))
                            . map (\y->read $ "(" ++ y ++ ")") . splitOn " -> ")
                            (lines input)

fall :: CaveMap -> CaveMap
fall cave  = maybe cave fall (fall1 (500,0) (Just cave))
  where 
    fall1 from cave@(Just (m,maxY))
      | fromY > maxY = Nothing
      | Map.member (500,0) m = Nothing
      | Map.notMember (down from) m = fall1 (down from) cave -- keep going downwards
      | Map.notMember (downAndLeft from) m = fall1 (downAndLeft from) cave
      | Map.notMember (downAndRight from) m = fall1 (downAndRight from) cave
      | otherwise = Just (Map.insert from Sand m, maxY)
      where fromY = snd from

down :: Coord -> Coord
down (fromX, fromY) = (fromX,fromY+1)

downAndLeft :: Coord -> Coord
downAndLeft (fromX, fromY) = (fromX-1,fromY+1)

downAndRight :: Coord -> Coord
downAndRight (fromX, fromY) = (fromX+1,fromY+1)


_input="498,4 -> 498,6 -> 496,6\n503,4 -> 502,4 -> 502,9 -> 494,9"

vector :: (Coord,Coord) -> [Coord]
vector ((fromX,fromY), (toX,toY)) 
  | fromX == toX = [(fromX,y) | y <- [(min fromY toY)..(max fromY toY)]]
  | fromY == toY = [(x,fromY) | x <- [(min fromX toX)..(max fromX toX)]]
  | otherwise = error "pair doesn't make sense"
