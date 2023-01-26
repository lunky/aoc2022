module Day16
    ( 
    day16
   ,day16b
   ,_input
    )
    where
import Data.List.Split (splitOn)
import qualified  Text.Parsec as Parsec
import Text.ParserCombinators.Parsec ((<|>))
import qualified Text.ParserCombinators.Parsec as P
import Data.Either (rights)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set


import Data.Tree

type Parser = Parsec.Parsec String ()

data State = Open | Closed deriving (Show, Eq)

data Valve = Valve {
  name::String 
  ,flowRate::Int 
  ,tunnels::[String] 
  ,state::State 
} deriving (Show)

instance Eq Valve where 
   a == b = name a == name b
   a /= b = name a /= name b

instance Ord Valve where 
  compare a b = compare (flowRate a) (flowRate b)
  
distance v1 v2 m = 0
  
open v = state v == Open
opened v = state v == Open
closed v = state v == Closed

explore valves = go "AA" Set.empty $ Map.fromList $ map (\y-> (name y, y)) valves

go curr s m = Node curr
                   (map (\y-> go y (Set.insert y s) m)
                          $ filter (`Set.notMember` s) $ tunnels $ m Map.! curr )
-- go curr s m = Node (curr,m Map.! curr) 
--                    (map (\y-> go y (Set.insert y s) m)
--                           $ filter (`Set.notMember` s) $ tunnels $ m Map.! curr )

    
day16 :: String -> Int 
day16 input = 0 

day16b :: String -> Int
day16b input = 0


-- Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
valve :: Parser Valve
valve = do
  _ <- Parsec.string "Valve "
  label <- Parsec.many1 (Parsec.noneOf " ")
  _ <- Parsec.string " has flow rate="
  flowRate <- Parsec.many1 Parsec.digit
  _ <- Parsec.try (Parsec.string "; tunnel leads to valve ")
   <|> Parsec.string "; tunnels lead to valves "
  valves <- Parsec.many1 (Parsec.noneOf "\n")
  return (Valve label (read flowRate) (splitOn ", " valves) Closed)

parse :: Parser a -> String -> Either Parsec.ParseError a
parse p = P.parse p ""

parseInput :: String -> [Valve]
parseInput input = rights $ parse valve <$> lines input

_input="Valve AA has flow rate=0; tunnels lead to valves DD, II, BB\nValve BB has flow rate=13; tunnels lead to valves CC, AA\nValve CC has flow rate=2; tunnels lead to valves DD, BB\nValve DD has flow rate=20; tunnels lead to valves CC, AA, EE\nValve EE has flow rate=3; tunnels lead to valves FF, DD\nValve FF has flow rate=0; tunnels lead to valves EE, GG\nValve GG has flow rate=0; tunnels lead to valves FF, HH\nValve HH has flow rate=22; tunnel leads to valve GG\nValve II has flow rate=0; tunnels lead to valves AA, JJ\nValve JJ has flow rate=21; tunnel leads to valve II"
