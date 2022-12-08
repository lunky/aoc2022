{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TupleSections #-}
module Day7
    ( 
    day7
   ,day7b
   ,_input
    )
    where
import qualified  Text.Parsec as Parsec
import Text.ParserCombinators.Parsec ((<|>))
import qualified Text.ParserCombinators.Parsec as P
import Data.Either (fromRight)
import Data.Maybe (fromJust)

import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.List (intercalate, foldl', inits, sort)
import Data.List.Split (splitOn)


type Parser = Parsec.Parsec String ()


type Name = String  
type Parent = String  
type Size = Int

data FItem = FFile Name Size Parent | FFolder Name [FItem] deriving (Show)

data FSItem = File Name Size | Folder Name [FSItem] deriving (Show)
    
data CommandOutput = Dir String | MFile String Int | CdCommand String 
                                              deriving (Eq,Show)

parse :: Parser a -> String -> Either Parsec.ParseError a
parse p = P.parse p ""

parseCd :: Parser [CommandOutput]
parseCd = do
  _ <- Parsec.string "$ cd " 
  cd <- Parsec.many1 (Parsec.noneOf ",\n\r")
  _ <- Parsec.char '\n' 
  return [CdCommand cd]

parseLs :: Parser [CommandOutput]
parseLs = do
  _ <- Parsec.string "$ ls\n" 
  Parsec.many (Parsec.try parseDirLine <|> Parsec.try parseLsLine)

parseLsLine :: Parser CommandOutput 
parseLsLine = do
  size <- Parsec.many1 Parsec.digit
  _ <- Parsec.char ' '
  file <- Parsec.many1 (Parsec.noneOf ",\n\r")
  _ <- Parsec.char '\n' 
  return (MFile file (read size))

parseDirLine :: Parser  CommandOutput
parseDirLine = do
  _ <- Parsec.string "dir " 
  dir <- Parsec.many1 (Parsec.noneOf "\n")
  _ <- Parsec.string "\n" 
  return $ Dir dir

parseCommandOutput :: Parser [CommandOutput]
parseCommandOutput = do 
  cmds <- Parsec.many (Parsec.try parseCd <|> Parsec.try parseLs )
  return $ concat cmds

day7 :: String -> Int 
day7 input = sum $ map snd $ filter (\(a,b)->b<100_000) $ Map.toList $ day7' input

day7' :: String -> Map [Char] Int
day7' input =  foldr (\(a,b) m -> Map.insertWith (+) a b m)  Map.empty $ 
              concatMap (\(x,y)-> map (,y) $ getParents x) $  snd $ readTree $ parseInput input

day7b :: String -> Int
day7b input = head $ filter (>= toFree) $ sort $ map snd $ Map.toList m
  where root = fromJust $ Map.lookup "" m
        toFree = 30_000_000 - (70_000_000 - root)
        m = day7' input

parseInput :: String -> [CommandOutput]
parseInput input = fromRight [] $ parse parseCommandOutput input

readTree :: [CommandOutput] -> (String, [(String,Int)])
readTree = foldl' (\(curr,stack) y -> 
                                      case y of 
                                        (CdCommand newPath) -> (cd curr newPath ,stack) 
                                        (MFile name size) -> (curr,(curr++"/" ++ name,size):stack) 
                                        _ -> (curr,stack)
                                      ) ("",[]) 

cd :: String -> String -> String
cd curr "/" = ""
cd "/" ".." = ""
cd curr ".." = getParent curr 
cd curr path = curr ++ "/" ++ path

getParent :: String -> String
getParent path = intercalate "/" $ init $ splitOn "/" path 

getParents :: String -> [String]
getParents path = init $ tail $ map (intercalate "/") $ inits $ splitOn "/" path

_input="$ cd /\n$ ls\ndir a\n14848514 b.txt\n8504156 c.dat\ndir d\n$ cd a\n$ ls\ndir e\n29116 f\n2557 g\n62596 h.lst\n$ cd e\n$ ls\n584 i\n$ cd ..\n$ cd ..\n$ cd d\n$ ls\n4060174 j\n8033020 d.log\n5626152 d.ext\n7214296 k\n"
