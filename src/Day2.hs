module Day2
    ( 
    day2
   ,day2b
   ,_input
    )
    where
    
day2 :: String -> Int 
day2 input = sum $ map (\(a,b)-> score (a, b)) $ parseInput input

day2b :: String -> Int
day2b input = sum $ map (\(a,b)-> scoreb (a, b)) $ parseInputb input



scoreb (them, outcome ) = case toWin them outcome  of
                           Rock     -> you + 1
                           Paper    -> you + 2
                           Scissors -> you + 3
  where you = scoreOutcome outcome

toWin Rock Win = Scissors   -- X lose
toWin Paper Win = Rock
toWin Scissors Win = Paper

toWin Rock Draw = Rock    -- Y Draw
toWin Paper  Draw = Paper 
toWin Scissors  Draw = Scissors

toWin Rock Lose = Paper -- Z win
toWin Paper Lose = Scissors 
toWin Scissors Lose = Rock

scoreOutcome Lose = 0
scoreOutcome Draw = 3
scoreOutcome Win = 6

data Outcome = Win | Lose | Draw deriving Show
data Shape = Rock | Paper | Scissors deriving Show


winner :: Shape -> Shape -> Outcome
winner Rock Rock = Draw
winner Paper Scissors = Lose
winner Scissors Paper = Win

winner Rock Scissors = Win
winner Paper Paper = Draw
winner Scissors Rock = Lose

winner Rock Paper = Lose
winner Paper Rock = Win
winner Scissors Scissors = Draw

myScore Rock = 1
myScore Paper = 2
myScore Scissors = 3


score :: (Shape, Shape) -> Int
score (them, you) = case winner them you of
                       Win  -> myhand + 6
                       Draw -> myhand + 3
                       _    -> myhand + 0
  where myhand = myScore you

parseInput input = map (\y-> (shape $ head y, shape $ last y)) $ lines input

parseInputb input = map (\y-> (shape $ head y, hand $ last y)) $ lines input

_input="A Y\nB X\nC Z"

--
class Hand a where
  hand :: a -> Outcome

instance Hand Char where
  hand 'X' = Win
  hand 'Y' = Draw
  hand 'Z' = Lose

class GameShape a where
  shape :: a -> Shape

instance GameShape Char where
  shape 'A' = Rock
  shape 'B' = Paper
  shape 'C' = Scissors
  shape 'X' = Rock
  shape 'Y' = Paper
  shape 'Z' = Scissors
