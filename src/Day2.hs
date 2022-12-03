module Day2
    ( 
      day2
     ,day2b
     ,_input
    )
    where

data Outcome = Win | Lose | Draw deriving Show
data Shape = Rock | Paper | Scissors deriving Show

class Hand a where
  hand :: a -> Outcome

instance Hand Char where
  hand 'Z' = Win
  hand 'Y' = Draw
  hand 'X' = Lose

class GameShape a where
  shape :: a -> Shape

instance GameShape Char where
  shape 'A' = Rock
  shape 'B' = Paper
  shape 'C' = Scissors
  shape 'X' = Rock
  shape 'Y' = Paper
  shape 'Z' = Scissors
    
day2 :: String -> Int 
day2 input = sum $ map score $ parseInput input

day2b :: String -> Int
day2b input = sum $ map scoreb $ parseInputb input

score :: (Shape, Shape) -> Int
score (them, you) = case winner them you of
                       Win  -> myhand + 6
                       Draw -> myhand + 3
                       _    -> myhand + 0
  where myhand = myScore you
        myScore Rock = 1
        myScore Paper = 2
        myScore Scissors = 3

        winner :: Shape -> Shape -> Outcome
        winner Rock Rock = Draw
        winner Paper Rock= Lose
        winner Scissors Rock= Win
        winner Rock Paper = Win
        winner Paper Paper = Draw
        winner Scissors Paper = Lose
        winner Rock Scissors = Lose
        winner Paper Scissors = Win
        winner Scissors Scissors = Draw

scoreb :: (Shape, Outcome) -> Int
scoreb (them, outcome ) = case toWin them outcome  of
                           Rock     -> you + 1
                           Paper    -> you + 2
                           Scissors -> you + 3
  where you = scoreOutcome outcome
        scoreOutcome Lose = 0
        scoreOutcome Draw = 3
        scoreOutcome Win = 6
        toWin Rock Win = Paper -- X lose
        toWin Paper Win = Scissors
        toWin Scissors Win = Rock

        toWin Rock Draw = Rock    -- Y Draw
        toWin Paper  Draw = Paper 
        toWin Scissors  Draw = Scissors

        toWin Rock Lose = Scissors -- Z win
        toWin Paper Lose = Rock
        toWin Scissors Lose = Paper 

parseInputb :: String -> [(Shape, Outcome)]
parseInputb input = map (\y-> (shape $ head y, hand $ y!!2)) $ lines input

parseInput :: String -> [(Shape, Shape)]
parseInput input = map (\y-> (shape $ head y, shape $ y!!2)) $ lines input

_input="A Y\nB X\nC Z"

--
