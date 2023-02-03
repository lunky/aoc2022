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
import Data.Maybe (isJust)

type Parser = Parsec.Parsec String ()

data MonkeyJob = Value String Int | Equation String String Char String | H String MonkeyJob Char MonkeyJob deriving (Show,Eq)

opposite '+' = '-'
opposite '-' = '+'
opposite '*' = '/'
opposite '/' = '*'


day21b' assignments = (human, eval $ go (name nonHuman) assignments)
  where
    (Equation "root" lhs _ rhs) = assignments Map.! "root"
    leftLeg = go lhs assignments
    rightLeg = go rhs assignments
    (human,nonHuman) = if isJust ( find "humn" leftLeg) then (leftLeg , rightLeg) else (rightLeg, leftLeg)
    go key m = case m Map.! key of
                 Equation key lhs op rhs -> H key (go lhs m) op (go rhs m)
                 Value key v -> Value key v

assignments input = Map.fromList $ map ((\(Right x)->(name x,x)) . parse monkeyJob) $ parseInput input

unwind :: (MonkeyJob, Int) -> Int
unwind (job,acc) = go (job, acc)
  where 
    go :: (MonkeyJob, Int) -> Int
    go (Value "humn" _, answer) = answer
    go (H tag leftLeg op rightLeg, acc) = 
      if leftLeg == human || op == '+' || op == '*' then  -- commutative
        go (human, (operation $ opposite op) acc (eval nonHuman) )
      else
        go (human, operation op (eval nonHuman) acc )
      where 
        (human,nonHuman) = if isJust ( find "humn" leftLeg) then (leftLeg , rightLeg) else (rightLeg, leftLeg)

operation o = case o of
                        '+'  -> (+)
                        '-'  -> (-)
                        '*'  -> (*)
                        '/'  -> div

eval :: MonkeyJob -> Int
eval m = case m of
           H tag lhs op rhs -> operation op (eval lhs) (eval rhs)
           Value tag v -> v

find :: String -> MonkeyJob -> Maybe MonkeyJob
find who x = find' who [x]
  where 
    find' who [] = Nothing
    find' who (x@(Value tag _):xs) 
      | tag == who = Just x
      | otherwise = find' who xs
    find' who (x@(H tag lhs _ rhs):xs)
      | tag == who = Just x
      | otherwise = find' who (lhs:(rhs:xs))


day21 :: String -> Int
day21 input = go "root" assignments
   where
    go key m = case m Map.! key of
                 Equation tag lhs op rhs -> operation op (go lhs m) (go rhs m)
                 Value tag v -> v
    assignments = Map.fromList $ map ((\(Right x)->(name x,x)) . parse monkeyJob) $ parseInput input


day21b :: String -> Int
day21b input = unwind $ day21b' $ assignments input

parse :: Parser a -> String -> Either Parsec.ParseError a
parse p = P.parse p ""

name (Value tag _) = tag
name (H tag _ _ _) = tag
name (Equation tag _ _ _) = tag


monkeyJob :: Parser MonkeyJob
monkeyJob = do
  Parsec.try monkeyJobValue <|> Parsec.try monkeyJobEquation

monkeyJobEquation :: Parser MonkeyJob
monkeyJobEquation = do
  tag <- Parsec.many1 (Parsec.noneOf ":")
  _ <- Parsec.string ": "
  lhs <- Parsec.many1 (Parsec.noneOf " ")
  _ <- Parsec.char ' '
  operator <- monkeyOperation
  _ <- Parsec.char ' '
  rhs <- Parsec.many1 (Parsec.noneOf " ")
  return $ Equation tag lhs operator rhs

monkeyOperation :: Parser Char
monkeyOperation  = do
  Parsec.try ( Parsec.char '+'
                <|> Parsec.char '-'
                <|> Parsec.char '/'
                <|> Parsec.char '*' )

monkeyJobValue :: Parser MonkeyJob
monkeyJobValue = do
  tag <- Parsec.many1 (Parsec.noneOf ":")
  _ <- Parsec.string ": "
  v <- Parsec.many1 Parsec.digit
  return $ Value tag (read v)

parseInput = lines
_input="root: pppw + sjmn\ndbpl: 5\ncczh: sllz + lgvd\nzczc: 2\nptdq: humn - dvpt\ndvpt: 3\nlfqf: 4\nhumn: 5\nljgn: 2\nsjmn: drzm * dbpl\nsllz: 4\npppw: cczh / lfqf\nlgvd: ljgn * ptdq\ndrzm: hmdt - zczc\nhmdt: 32\n"
-- wrong answer 9176923712089
--              28379346560301
--
--              3221245824363
