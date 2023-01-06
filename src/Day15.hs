{-# Language NumericUnderscores #-}
module Day15
    ( 
    day15
   ,day15b
   ,_input
    )
    where
import Data.Char (isDigit)
import Data.List.Split (splitOneOf)
import Data.List (nub, (\\))

type Coord = (Int,Int)
type Sensor = (Coord,Coord)
    
day15 :: Int -> String -> Int 
day15 row input = length $ filter id $ map (\curr -> any (inBounds curr) telemetry) $ line (minX,row) (maxX,row) \\ beacons
  where telemetry = parseInput input
        beacons = nub $ map snd telemetry
        (minX,maxX) = maxXBounds telemetry


answer :: Coord -> Int
answer (x,y) = x * _defaultFrequency + y

_defaultFrequency :: Int
_defaultFrequency = 4_000_000

parseInput :: String -> [Sensor]
parseInput =  map ((\[a,b,c,d]->((a,b),(c,d))) 
                   . map read 
                   . splitOneOf ",:" 
                   .  filter (\y-> y==':' || y=='-' || isDigit y || y==',' )) 
                   . lines 


maxXBounds :: [Sensor] -> (Int,Int)
maxXBounds = foldr (\y (accLeft, accRight) -> (if farLeft y < accLeft then farLeft y else accLeft 
                                                      ,if farRight y > accRight then farRight y else accRight)) (0,0) 
  where farLeft (sensor,beacon) = fst $ if sensor < beacon then left sensor (distance sensor beacon) else beacon
        farRight (sensor,beacon) = fst $ if sensor > beacon then right sensor (distance sensor beacon) else beacon


inBounds :: Coord -> Sensor -> Bool
inBounds p (sensor,beacon) = distance sensor p <= distance sensor beacon

right :: Coord -> Int -> Coord
right (px,py) n = (px+n,py) 

left :: Coord -> Int -> Coord
left (px,py) n = (px-n,py) 

line :: Coord -> Coord -> [Coord]
line (fromX,fromY) (toX,toY) 
  | fromX == toX = [(fromX,y) | y <- [(min fromY toY)..(max fromY toY)],y<4000000]
  | fromY == toY = [(x,fromY) | x <- [(min fromX toX)..(max fromX toX)],x<4000000]
  | otherwise = error "pair doesn't make sense"

distance :: Coord -> Coord -> Int
distance (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)

_input="Sensor at x=2, y=18: closest beacon is at x=-2, y=15\nSensor at x=9, y=16: closest beacon is at x=10, y=16\nSensor at x=13, y=2: closest beacon is at x=15, y=3\nSensor at x=12, y=14: closest beacon is at x=10, y=16\nSensor at x=10, y=20: closest beacon is at x=10, y=16\nSensor at x=14, y=17: closest beacon is at x=10, y=16\nSensor at x=8, y=7: closest beacon is at x=2, y=10\nSensor at x=2, y=0: closest beacon is at x=2, y=10\nSensor at x=0, y=11: closest beacon is at x=2, y=10\nSensor at x=20, y=14: closest beacon is at x=25, y=17\nSensor at x=17, y=20: closest beacon is at x=21, y=22\nSensor at x=16, y=7: closest beacon is at x=15, y=3\nSensor at x=14, y=3: closest beacon is at x=15, y=3\nSensor at x=20, y=1: closest beacon is at x=15, y=3"

{-- part 2
if there's only 1 spot, it's completely surrounded by the diamond shapes created by the 
beacons , let's check around the edges of the diamonds created by the beacons, we'll find
it there
--}

day15b :: Int -> String -> Int
day15b xyMax input =  answer $ head $ dropWhile test
                        $ filter (\(a,b)->a>0 && b >0 && a< xyMax && b< xyMax ) 
                        $ concatMap checkAdjacentEdges  bounds
  where test p = any (inBounds p) bounds
        bounds = parseInput input

checkAdjacentEdges :: Sensor -> [Coord]
checkAdjacentEdges s = concat 
                         $ (\[a,b,c,d]-> [line' a b,line' b c,line' c d,line' d a]) 
                         $ corners s
  where 
    -- these are the points on the diamonds, the same "Manhattan" 
    -- distance from the sensor as between beacon and sensor (plus 1 in this case)
    corners (s@(sx,sy),b@(bx,by)) = [ (sx - d,sy),
                                      (sx ,sy + d),
                                      (sx + d,sy),
                                      (sx ,sy - d)]
       where d = distance s b + 1 -- one more than the actual edges (so we can test them)
    line' (sx,sy) (ex,ey)         -- these are specialty 45 degree lines
       | sx < ex && sy > ey = [(x,sy+sx-x) | x <- [sx..ex]]
       | sx < ex && sy < ey = [(x,sy-sx+x) | x <- [sx..ex]]
       | sx > ex && sy > ey = [(x,sy-sx+x) | x <- [ex..sx]]
       | sx > ex && sy < ey = [(x,sy+sx-x) | x <- [ex..sx]]

