module Day5
    ( 
    day5
   ,day5b
   ,_input
    )
    where
import Data.List
import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.Either
import Data.List.Split     
import qualified  Text.Parsec as Parsec
import qualified Text.ParserCombinators.Parsec as P

type Parser = Parsec.Parsec String ()
parse :: Parser a -> String -> Either Parsec.ParseError a
parse p = P.parse p ""

parseRule :: Parser (Int,Int,Int)
parseRule = do
  _ <- Parsec.string "move " 
  what <- Parsec.many1 Parsec.digit
  _ <- Parsec.string " from "
  from <- Parsec.many1 Parsec.digit
  _ <- Parsec.string " to "
  to <- Parsec.many1 Parsec.digit
  return (read what,read from - 1,read to - 1) 



day5 :: String -> String
day5 input = V.toList $ V.map head 
  $ uncurry (foldr move) 
  $ parseInput input

day5b :: String -> String
day5b input = V.toList $ V.map head 
  $ uncurry (foldr moveb) 
  $ parseInput input

moveb (what,from,to) vec  = do
  let taken = take what $ (V.!) vec from
  let left = drop what $ (V.!) vec from
  let ys = (V.!) vec to
  (vec V.// [(from,left),(to,taken ++ ys)]) 

move (what,from,to) vec
  | what == 0 = vec
  | otherwise = do
  let (x:xs) = (V.!) vec from
  let ys = (V.!) vec to
  move (what-1, from, to) (vec V.// [(from,xs),(to,x:ys)]) 
  




parseInput input = (stacks, reverse $ rights $ parse parseRule <$> lines bottom)
    where 
      stacks = V.fromList $ map (filter (/=' ')) $ transpose
                   $ map (map (!!1) . chunksOf 4) 
                   $ init $ lines top
      [top,bottom] = splitOn "\n\n" input

moveCrate (crates,(what,from,to))
  | what == 0 = (crates, (what,from,to))
  | what > 0 = moveCrate (crates, (what-1,from,to))

_input="    [D]    \n[N] [C]    \n[Z] [M] [P]\n1   2   3 \n\nmove 1 from 2 to 1\nmove 3 from 1 to 3\nmove 2 from 2 to 1\nmove 1 from 1 to 2"
{--
    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
  --}
