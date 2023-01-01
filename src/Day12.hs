{-# Language TupleSections #-}
module Day12
    ( 
    day12
   ,day12b
   ,_input
    )
    where

import Data.Array
import Prelude hiding (Right, Left)
import Data.Sequence (viewl, ViewL (..), (><))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Tree
import Data.Char (ord)
import Data.Maybe (catMaybes, fromJust)

type Board = Array (Int, Int) Char
type Coord = (Int,Int)


parseInput :: String -> Board
parseInput input = listArray ((0,0), (length xs -1, length (head xs) - 1 )) (concat xs)
  where xs = lines input

around :: Board -> Coord -> [Coord]
around a c = filter inside $ map (\y-> y c) [above,below,left,right] 
  where inside x = inRange (bounds a) x

above :: Coord -> Coord
above (y,x) = (y-1,x)

below :: Coord -> Coord
below (y,x) = (y+1,x)

left :: Coord -> Coord
left (y,x) = (y,x-1)

right :: Coord -> Coord
right (y,x) = (y,x+1)

explore :: Coord -> Board -> Tree (Coord, Char)
explore start a = go start Set.empty
  where go c s = Node (c,a!c) (map (\y-> go y (Set.insert y s)) 
                  $ filter (`Set.notMember` s) $ valid a c)

breadthFirstSearchUnseen:: Ord r => (a -> r) -> (a -> Bool) -> [Tree a] -> Maybe [a]
breadthFirstSearchUnseen repr p = combine Set.empty Seq.empty []
    where
        combine seen queued ancestors unseen =
            go
                (seen  `Set.union` (Set.fromList . map (repr . rootLabel) $ unseen))
                (queued ><         (Seq.fromList . map (ancestors,) $ unseen))
        go seen queue =
            case viewl queue of
                EmptyL -> Nothing
                (ancestors, Node a bs) :< queued ->
                    if p a
                    then Just . reverse $ ancestors'
                    else combine seen queued ancestors' $ unseen bs
                    where
                        ancestors' = a:ancestors
                        unseen = filter (flip Set.notMember seen . repr . rootLabel)

valid :: Board -> Coord -> [Coord]
valid a c = filter (\y-> greater a y c) $ around a c

greater :: Board -> Coord -> Coord -> Bool
greater a c x = a!x == 'S' 
                     || (a!c == 'E' && (a!x) == 'z')
                     || ((ord (a!c) - ord (a!x)) <= 1 && (a!c /= 'E'))
                     || (ord (a!c) - ord (a!x)) == 0

findStart :: Board -> Coord
findStart a = fst $ head $ filter (\(a,b)->b=='S') $ assocs a

findStarts :: Board -> [Coord]
findStarts a = map fst $ filter (\(_,b)->b=='a' || b=='S') $ filter (\((y,x),_)->x==maxx || y==maxy || x==0 || y==0) $ assocs a
  where (_,(maxy,maxx)) = bounds a

findEnd :: Board -> Coord
findEnd a = fst $ head $ filter (\(a,b)->b=='E') $ assocs a

day12 :: String -> Int 
day12 input =  fromJust $ day12' input

day12' :: String -> Maybe Int
day12' input = (\y-> length y - 1) 
     <$> breadthFirstSearchUnseen id (\y->y==(findEnd a,'E')) [explore start a]
  where a = parseInput input
        start = findStart a

day12b' :: String -> [Maybe Int]
day12b' input = (\start ->  (\y-> length y - 1) <$> breadthFirstSearchUnseen id (\y->y==(findEnd a,'E')) [explore start a]) <$> findStarts a
  where a = parseInput input

day12b :: String -> Int
day12b input = minimum $ catMaybes $ day12b' input

_input :: String
_input="Sabqponm\nabcryxxl\naccszExk\nacctuvwj\nabdefghi"



{--
  01234567
0 Sabqponm
1 abcryxxl
2 accszExk
3 acctuvwj
4 abdefghi
--}
