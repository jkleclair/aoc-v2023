module Day4
  ( day4,
  )
where

import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List.Split ( splitOn )
import Paths_aoc_v2023 (getDataFileName)

type Card = ([Int], [Int])

-- find how many numbers matched
checkNumbers :: Card -> Int
checkNumbers (winningNumbers, myNumbers) = length (myNumbers `List.intersect` winningNumbers)

scoreCard :: Int -> Int
scoreCard matches
  | matches == 0 = 0
  | otherwise = 2 ^ (matches - 1)

parseId :: String -> Int
parseId idStr = case words idStr of
  [_, cardId] -> read cardId :: Int
  _ -> 0

parseNumbers :: String -> Int
parseNumbers numbers = case splitOn " | " numbers of
  [winningNumbers, myNumbers] -> matches
    where
      parsedWinningNumbers = map read (words winningNumbers) :: [Int]
      parsedMyNumbers = map read (words myNumbers) :: [Int]
      matches = checkNumbers (parsedWinningNumbers, parsedMyNumbers)
  _ -> 0

parseCard :: String -> (Int, Int)
parseCard card = case splitOn ": " card of
  [idStr, numbers] -> (cardId, parsedNumbers)
    where
      cardId = parseId idStr
      parsedNumbers = parseNumbers numbers
  _ -> (0, 0)

parseCards :: [String] -> Map Int Int -> Map Int Int
parseCards (cur:rest) acc = parseCards rest (Map.insert cardId matches acc)
  where
    (cardId, matches) = parseCard cur
parseCards [] acc = acc

parseCards' :: [Int] -> Map Int Int -> Int -> Int
parseCards' (curId:rest) cardSet acc = parseCards' (newIds ++ rest) cardSet (acc + 1)
  where
    matches = cardSet Map.! curId 
    newIds = [(curId + 1)..(curId + matches)]
parseCards' [] _ acc = acc

day4 :: IO ()
day4 = do
  filePath <- getDataFileName "data/day4.txt"
  inFile <- readFile filePath
  let cards = lines inFile
  let parsedCards = parseCards cards Map.empty
  let scored = Map.map scoreCard parsedCards

  putStrLn "Day 4"
  putStr "Part 1: "
  print (sum scored)

  putStr "Part 2: "
  print (parseCards' (Map.keys parsedCards) parsedCards 0)