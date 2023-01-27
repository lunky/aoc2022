module Day21
    ( 
    day21
   ,day21b
   ,_input
    )
    where

import qualified  Text.Parsec as Parsec
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec ((<|>))
import qualified Data.Map as Map
import Data.Either (rights)

type Parser = Parsec.Parsec String ()
    
day21 :: String -> Int 
day21 input = go "root" assignments
   where 
     go key m = case m Map.! key of
                  Equation lhs operation rhs -> operation (go lhs m) (go rhs m)
                  Value v -> v
     assignments = Map.fromList $ rights $ map (parse monkeyAssignment) $ parseInput input

day21b :: String -> Int
day21b input = 0

parse :: Parser a -> String -> Either Parsec.ParseError a
parse p = P.parse p ""

type Operation = Int -> Int -> Int

data MonkeyJob = Value Int | Equation String Operation String 

instance Show MonkeyJob where 
  show (Value a) = show a
  show (Equation a b c) = show a ++ " op " ++ show c


monkeyAssignment :: Parser (String, MonkeyJob)
monkeyAssignment = do 
  lhs <- Parsec.many1 (Parsec.noneOf ":") 
  _ <- Parsec.string ": "
  rhs <- monkeyJob
  return (lhs,rhs)

monkeyJob :: Parser MonkeyJob
monkeyJob = do 
  Parsec.try monkeyJobValue <|> Parsec.try monkeyJobEquation

monkeyJobEquation :: Parser MonkeyJob
monkeyJobEquation = do
  lhs <- Parsec.many1 (Parsec.noneOf " ") 
  _ <- Parsec.char ' '
  operator <- monkeyOperation
  _ <- Parsec.char ' '
  rhs <- Parsec.many1 (Parsec.noneOf " ") 
  return $ Equation lhs operator rhs

monkeyOperation :: Parser Operation 
monkeyOperation  = do
  o <- Parsec.try ( Parsec.char '+'
                <|> Parsec.char '-' 
                <|> Parsec.char '/'
                <|> Parsec.char '*' )
  return $ case o of 
                  '+' -> (+)
                  '-' -> (-)
                  '*' -> (*)
                  '/' -> div
monkeyJobValue :: Parser MonkeyJob
monkeyJobValue = do
  v <- Parsec.many1 Parsec.digit 
  return $ Value $ read v
  
parseInput = lines
_input="root: pppw + sjmn\ndbpl: 5\ncczh: sllz + lgvd\nzczc: 2\nptdq: humn - dvpt\ndvpt: 3\nlfqf: 4\nhumn: 5\nljgn: 2\nsjmn: drzm * dbpl\nsllz: 4\npppw: cczh / lfqf\nlgvd: ljgn * ptdq\ndrzm: hmdt - zczc\nhmdt: 32\n"
