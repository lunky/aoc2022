module Day11
    ( 
    day11
   ,day11b
   ,_input
    )
    where
import qualified  Text.Parsec as Parsec
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec ((<|>))
import Data.Either
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M
import Data.List (foldl', delete, sort)

type Parser = Parsec.Parsec String ()
    
type Action = Int
type Test = Int
type Monkeys = IntMap Monkey
type OperationB = Int -> Int

data Monkey = Monkey {
              _index :: Int, 
              _startingItems :: [Int],   
              _operation :: OperationB, 
              test :: Test,
              _ifTrue :: Action,
              _ifFalse :: Action,
              counter :: Int }

instance Show Monkey where
 show (Monkey i j k l m n c) = show (i, j, "OperationB", l, m, n, c)


day11 :: String -> Int 
day11 = day11' 20 day11a' 

day11b :: String -> Int -> Int 
day11b input x = day11' x day11b' input


day11' :: Int -> (Monkeys -> Monkeys) -> String -> Int
day11' x f input = product $ take 2 $ reverse $ sort $ map (counter.snd) $ M.toList
                               $ iterate f ( M.fromList $ parseInput input) !! x

processMonkey :: Monkey -> [(Int, Int, Int, Int)]
processMonkey (Monkey monkeyNum startingItems operation test trueTest falseTest _) = 
      reverse $ 
      foldl' (\acc item -> do
         let output =  operation item 
         let newWorryLevel = output `div` 3
         if newWorryLevel `mod` test == 0 
         then (monkeyNum, item, newWorryLevel , trueTest):acc 
         else (monkeyNum, item, newWorryLevel, falseTest):acc
         ) [] startingItems

processMonkeyb' :: Monkey -> Monkeys -> Test -> Monkeys
processMonkeyb' (Monkey monkeyNum startingItems operation test trueTest falseTest _) m divisor= 
    foldl' (\acc item-> do
        let newWorryLevel = operation item 
        let i = if newWorryLevel `mod` test == 0 then trueTest else falseTest
        M.adjust (`addStartingItems` (newWorryLevel `mod` divisor)) i
          $ M.adjust (`removeStartingItems` item) monkeyNum acc 
      ) m startingItems
     
day11a' :: Monkeys -> Monkeys
day11a' monkeys = foldl' (\n index -> 
                   updateStartingItems (processMonkey (n M.! index)) n 
                  ) monkeys (M.keys monkeys)

day11b' :: Monkeys -> Monkeys
day11b' monkeys = foldl' (\m index -> 
                     processMonkeyb' (m M.! index) m common_divisor
                   ) monkeys (M.keys monkeys)
  where common_divisor = product $ map (test.snd) $ M.toList monkeys
    -- use a common divisor to `mod` against later so we don't have to keep multiplying
    -- giant numbers


updateStartingItems :: [(Int, Int, Int, Int)] -> Monkeys -> Monkeys
updateStartingItems si m = foldl' (\acc (fromMonkey, oldItem, startingItems,toMonkey) -> 
                              M.adjust (`addStartingItems` startingItems) toMonkey
                              $!  M.adjust (`removeStartingItems` oldItem) fromMonkey 
                               acc
                            ) m si

addStartingItems :: Monkey -> Int -> Monkey
addStartingItems (Monkey n startingItems o t tT fT c) item 
            = Monkey n (item : startingItems ) o t tT fT c

removeStartingItems :: Monkey -> Int -> Monkey
removeStartingItems (Monkey n startingItems o t tT fT c) item
            = Monkey n (delete item startingItems) o t tT fT (c+1)

_input="Monkey 0:\n  Starting items: 79, 98\n  Operation: new = old * 19\n  Test: divisible by 23\n    If true: throw to monkey 2\n    If false: throw to monkey 3\n\nMonkey 1:\n  Starting items: 54, 65, 75, 74\n  Operation: new = old + 6\n  Test: divisible by 19\n    If true: throw to monkey 2\n    If false: throw to monkey 0\n\nMonkey 2:\n  Starting items: 79, 60, 97\n  Operation: new = old * old\n  Test: divisible by 13\n    If true: throw to monkey 1\n    If false: throw to monkey 3\n\nMonkey 3:\n  Starting items: 74\n  Operation: new = old + 3\n  Test: divisible by 17\n    If true: throw to monkey 0\n    If false: throw to monkey 1"

--- parsing types --

parseInput :: String -> [(Int,Monkey)]
parseInput input = map keyMonkey $ fromRight [] $ parse monkeys input
  where keyMonkey :: Monkey -> (Int,Monkey)
        keyMonkey m@(Monkey n _ _ _ _ _ _) = (n,m)


parse :: Parser a -> String -> Either Parsec.ParseError a
parse p = P.parse p ""

operationMul' :: Parser OperationB
operationMul' = do 
  _ <- Parsec.string "new = old * old\n"
  return (\y -> y * y)

operationMul :: Parser OperationB
operationMul = do 
  _ <- Parsec.string "new = old * "
  x <- Parsec.many1 Parsec.digit 
  _ <- Parsec.char '\n' 
  return (* read x)

operationAdd :: Parser OperationB
operationAdd = do 
  _ <- Parsec.string "new = old + "
  x <- Parsec.many1 Parsec.digit 
  _ <- Parsec.char '\n' 
  return (+ read x)

integer :: Parser Int
integer = do
  num <- Parsec.many1 Parsec.digit 
  return $ read num 

mySeparator :: Parsec.Parsec String () ()
mySeparator = do
  Parsec.spaces
  Parsec.char ','
  Parsec.spaces

monkeys :: Parser [Monkey]
monkeys = do Parsec.sepBy monkey (Parsec.try (Parsec.string "\n\n"))

monkey :: Parser Monkey 
monkey = do
  _ <- Parsec.string "Monkey "
  monkeyNum <- Parsec.many1 Parsec.digit 
  _ <- Parsec.string ":\n" 
  _ <- Parsec.string "  Starting items: "
  startingItems <- Parsec.sepBy integer (Parsec.try mySeparator)
  _ <- Parsec.string "\n  Operation: "
  operation <- Parsec.try operationMul <|> Parsec.try operationAdd <|> Parsec.try operationMul'
  _ <- Parsec.string "  Test: divisible by "
  test <- Parsec.many1 (Parsec.noneOf "\n") 
  _ <- Parsec.char '\n' 
  _ <- Parsec.string "    If true: throw to monkey "
  trueTest <- Parsec.many1 (Parsec.noneOf "\n") 
  _ <- Parsec.char '\n' 
  _ <- Parsec.string "    If false: throw to monkey "
  falseTest <- Parsec.many1 (Parsec.noneOf "\n") 
  return (Monkey (read monkeyNum) (map fromIntegral startingItems) operation (read test) (read trueTest) (read falseTest) 0)
