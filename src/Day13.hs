module Day13
    ( 
    day13
   ,day13b
   ,_input
   ,inOrder'
   ,p
    )
    where
import Data.List.Split (splitOn)
import Data.List (sort, elemIndex)
import Data.Maybe (fromJust)
import qualified  Text.Parsec as Parsec
import Text.ParserCombinators.Parsec ((<|>))
import qualified Text.ParserCombinators.Parsec as P

type Parser = Parsec.Parsec String ()

data LI = Item Int | List [LI]
  deriving (Read, Show, Eq)

instance Ord LI where
    compare (Item i1) (Item i2) = compare i1 i2
    compare (List xs) (List ys) = compare xs ys
    compare (List xs) (Item y) = compare (List xs) (List [Item y])
    compare (Item x) (List ys) = compare (List [Item x]) (List ys)

day13 :: String -> Int 
day13 input = sum $ 
                map fst $ 
                filter snd $ 
                zip [1..] $ 
                map (inOrder . (\[a,b]->(p a,p b)) . lines) $ 
                parseInput input

day13b :: String -> Int
day13b input = separatorIndex (p "[[2]]") * separatorIndex (p "[[6]]")
        where sorted = sort $ map p $ concatMap lines $ splitOn "\n\n" (input ++ dividers)
              separatorIndex el = fromJust (elemIndex el sorted) + 1
              dividers = "\n[[2]]\n[[6]]\n"

parseItem :: Parser LI
parseItem = do
  item <- Parsec.many1 Parsec.digit 
  return $ List [Item $ read item]

parseItems :: Parser LI
parseItems = do
  _ <- Parsec.char '['
  items <- Parsec.sepBy (Parsec.many Parsec.digit) (Parsec.char ',')
  _ <- Parsec.char ']'
  return $ List $ map read items

p xs = case parse parseList xs of 
        (Right x) -> x
        _ -> error "Did not parse"

parseList :: Parser LI
parseList = do
  _ <- Parsec.char '['
  items <- Parsec.sepBy (Parsec.many1 (Parsec.try parseList <|> Parsec.try parseItems <|> Parsec.try parseItem )) (Parsec.char ',')
  _ <- Parsec.char ']'
  return $ List $ concat items

inOrder  xs = case inOrder' xs of 
                LT -> True
                GT -> False
                _ -> error "should not be EQ at this level"

inOrder' :: (LI,LI) -> Ordering
inOrder' (Item a,Item b) = compare a b
inOrder' (List as,List bs) = compare as bs
inOrder' (Item a,List bs) = compare (List [Item a]) (List bs)
inOrder' (List as,Item b) = compare (List as) (List [Item b])

parse :: Parser a -> String -> Either Parsec.ParseError a
parse p = P.parse p ""

parseInput = splitOn "\n\n" 
_input="[1,1,3,1,1]\n[1,1,5,1,1]\n\n[[1],[2,3,4]]\n[[1],4]\n\n[9]\n[[8,7,6]]\n\n[[4,4],4,4]\n[[4,4],4,4,4]\n\n[7,7,7,7]\n[7,7,7]\n\n[]\n[3]\n\n[[[]]]\n[[]]\n\n[1,[2,[3,[4,[5,6,7]]]],8,9]\n[1,[2,[3,[4,[5,6,0]]]],8,9]"
