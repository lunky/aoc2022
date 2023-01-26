{-# LANGUAGE NumericUnderscores #-}
module Day17
    ( 
    day17
   ,day17b
   ,_input
    )
    where
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Data.List.Split (chunksOf)

type RockCoords = (Rock, [Point])
type Point = (Int,Int)
data Move = L | R | Down deriving (Show,Eq)
data Rock = Minus | Plus | LShape | IShape | Square deriving (Show, Eq)

getNext Minus = Plus
getNext Plus = LShape
getNext LShape = IShape
getNext IShape = Square
getNext Square = Minus -- make enumeration go on forever

day17 :: String -> Int 
day17 input = Set.foldr (\(_,y) acc -> max y acc) 0 $ day17' input 2022

day17' :: String -> Int -> Set Point
day17' input day = (\(_,s,_)->s) $ play input !! day

day17b :: Int -> Int -> String -> Int

day17b cycleStones leadStones input = leadRows + offsetRows + diffRows
  where cycleRows = getMaxY' (play input !! (cycleStones+leadStones)) - leadRows
        leadRows = getMaxY' $ play input !! leadStones
        cycles= (target - leadStones) `div` cycleStones
        offsetStones = cycles * cycleStones
        offsetRows = cycles * cycleRows
        diffRows = getMaxY' (play input !! (target - offsetStones - leadStones + leadStones))-leadRows
        getMaxY' i = getMaxY $ (\(_,s,_)->s) i

target = 1_000_000_000_000



getMaxY :: Set Point -> Int
getMaxY = Set.foldr (\(_,y) acc -> max y acc) 0 

height :: Rock -> Int
height Minus = 1
height Plus = 3
height LShape = 3
height IShape = 4
height Square = 2

saveRock :: Set Point -> RockCoords -> Set Point 
saveRock s (r,xs) 
  | any (`Set.member` s) xs = error ("should't be here. " ++ show r ++ " " ++ show s)
  |otherwise = foldr Set.insert s xs

leftWall :: Int
leftWall = 0

rightWall :: Int
rightWall = 8

leftMost :: Ord a => [(a,b)] -> a
leftMost points = minimum $ map fst points

rightMost :: Ord a => [(a,b)] -> a
rightMost points = maximum $ map fst points

nextRock :: RockCoords -> Set Point -> RockCoords
nextRock (shape,points) s = rock nextShape (3, 3 + getMaxY s + height nextShape )
  where nextShape = getNext shape


play :: String -> [(RockCoords, Set Point, [Move])]
play input = iterate go (rock Minus (3,4), makeFloor, directions)
  where
    directions = parseInput input
    go (currentRock, s, [] ) = go (currentRock, s, directions) -- repeat directions
    go (currentRock, s, d:ds) = do
      let curr = fromMaybe currentRock $ moveRock d currentRock s
      case moveRock Down curr s of
                   Nothing -> (nextRock curr (saveRock s curr), saveRock s curr, ds)
                   Just r -> go (r, s, ds)

makeFloor :: Set (Int,Int)
makeFloor = foldr Set.insert Set.empty [(x,0)| x <- [0..8]]

moveRock :: Move -> RockCoords -> Set Point -> Maybe RockCoords
moveRock L (rock, points) s 
  | leftMost newPoints <= leftWall = Nothing 
  | any (`Set.member` s) newPoints = Nothing 
  | otherwise = Just (rock, newPoints) 
  where newPoints = map left points

moveRock R (rock, points) s 
  | rightMost newPoints >= rightWall = Nothing 
  | any (`Set.member` s) newPoints = Nothing 
  | otherwise = Just (rock, newPoints) 
  where newPoints = map right points

moveRock Down (rock, points) s 
  | any (`Set.member` s) newPoints = Nothing 
  | otherwise = Just (rock, newPoints) 
  where newPoints = map down points

parseInput :: String -> [Move]
parseInput = map decode 
 where decode '>' = R 
       decode '<' = L

rock :: Rock -> Point -> RockCoords
rock Minus (x,y) =  (Minus,[(x,y),  (x+1,y),  (x+2,y),  (x+3,y)])

rock Plus (x,y) =   (Plus,[    (x+1,y  ),
                       (x,y-1),(x+1,y-1),(x+2,y-1),
                               (x+1,y-2)])
rock LShape (x,y) = (LShape,[                   (x+2,y  ),
                                                (x+2,y-1),
                              (x,y-2),(x+1,y-2),(x+2,y-2)])

rock IShape (x,y) = (IShape,[ (x,  y  ),
                              (x,  y-1),
                              (x,  y-2),
                              (x,  y-3)])

rock Square (x,y) = (Square,[ (x,  y  ),(x+1,  y),
                              (x,  y-1),(x+1,  y-1)])

down :: Point -> Point
down (x,y) = (x, y-1)

left :: Point -> Point
left (x,y) = (x-1, y)

right:: Point -> Point
right (x,y) = (x+1, y)

-- these are helper methods, just good for visualizing the data
_mapSet ((_,next), s, m) = do  
              chunksOf 7 [ if Set.member (x,y) s then '#' else '.' 
                                  | y <- [1..maxY], x <- [1..7]]
      where maxY = maximum ys
            l = Set.toList s
            ys = map snd l

_showSet :: (RockCoords, Set Point, [Move]) -> IO()
_showSet ((_,next), s, m) = do  
            mapM_ (\y->putStrLn (('|' : y) ++ "|")) $ 
              chunksOf 7 [ if Set.member (x,y) nextS then '#' else '.' 
                                  | y <- [maxY,maxY-1..1], x <- [1..7]]
            putStrLn "+-------+"
            putStrLn $ " remaining " ++ show m
      where nextS = Set.union s $ Set.fromList next
            maxY = maximum ys
            l = Set.toList nextS
            ys = map snd l

_removeFloor (_,s,_) = Set.filter (\(a,b)->b/=0) s

_input=">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"

-----------------------------------------------------------
-- todo: figure out how to make this findCycle work with the dataset
-- figure out a function that will find the cycle in the data
-----------------------------------------------------------
-- findCycle :: Eq a => [a] -> ([a],[a])
-- findCycle xxs = fCycle xxs xxs
--   where fCycle (x:xs) (_:y:ys)
--          | x == y              = fStart xxs xs
--          | otherwise           = fCycle xs ys
--         fCycle _      _        = (xxs,[]) -- not cyclic
--         fStart (x:xs) (y:ys)
--          | x == y              = ([], x:fLength x xs)
--          | otherwise           = let (as,bs) = fStart xs ys in (x:as,bs)
--         fLength x (y:ys)
--          | x == y              = []
--          | otherwise           = y:fLength x ys

