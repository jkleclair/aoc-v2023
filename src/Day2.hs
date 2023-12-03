module Day2
  ( day2,
  )
where

import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map
import Paths_aoc_v2023

type Color = [Char]

type Bag = Map Color Int

type Round = Map Color Int

type Game = [Round]

type Games = Map Int Game

gameOneBag :: Bag
gameOneBag = Map.fromList [("red", 12), ("green", 13), ("blue", 14)]

parseGames :: Games -> [[Char]] -> Games
parseGames acc (game : rest) = parseGames (Map.insert key val acc) rest
  where
    (key, val) = parseGame game
parseGames acc [] = acc

-- Returns (Id, Game) given the string: "Game {id}: {round1}; {round2}; ..."
parseGame :: [Char] -> (Int, Game)
parseGame game = case splitOn ": " game of
  [left, right] -> (gameId, rounds)
    where
      gameId = parseId left
      rounds = parseRounds (splitOn "; " right)
  _ -> (0, [])

-- Returns a list of rounds given the string "{round1}; {round2}; ..."
parseRounds :: [[Char]] -> [Round]
parseRounds rounds = case rounds of
  curRound : rest -> parseRound Map.empty colors : parseRounds rest
    where
      colors = splitOn ", " curRound
  [] -> []

-- Returns a round given a list of strings: ["{value1} {color1}", ...]
parseRound :: Round -> [[Char]] -> Round
parseRound acc colors = case colors of
  color : rest -> parseRound (Map.insert key val acc) rest
    where
      (key, val) = parseColor color
  [] -> acc

-- Returns id given the string: "Game {id}"
parseId :: [Char] -> Int
parseId gameId = case splitOn " " gameId of
  [_, idStr] -> read idStr :: Int
  _ -> 0

-- Returns (color, value) given the string: "{value} {color}"
parseColor :: [Char] -> (Color, Int)
parseColor color = case splitOn " " color of
  [valStr, key] -> (key, val)
    where
      val = read valStr :: Int
  _ -> ("", 0)

isGameValid :: Bag -> Game -> Bool
isGameValid bag = foldr (\cur acc -> isRoundValid cur bag && acc) True

isRoundValid :: Round -> Bag -> Bool
isRoundValid gameRound bag = Map.foldrWithKey (\color value acc -> isColorValid color value bag && acc) True gameRound

isColorValid :: Color -> Int -> Bag -> Bool
isColorValid color value bag = case Map.lookup color bag of
  Just bagVal -> value <= bagVal
  Nothing -> False

sumValidGameIds :: Map Int Bool -> Int
sumValidGameIds = Map.foldrWithKey (\gameId isValid acc -> if isValid then acc + gameId else acc) 0

addColorToMinBag :: Color -> Int -> Bag -> Bag
addColorToMinBag color val oldBag = case Map.lookup color oldBag of
  Just bagVal -> if bagVal < val then Map.insert color val oldBag else oldBag
  Nothing -> Map.insert color val oldBag

updateMinBagForRound :: Round -> Bag -> Bag
updateMinBagForRound gameRound bag = Map.foldrWithKey addColorToMinBag bag gameRound

getMinBagForGame :: Game -> Bag
getMinBagForGame = foldr updateMinBagForRound Map.empty

getPowerForBag :: Bag -> Int
getPowerForBag = Map.foldr (*) 1

day2 :: IO ()
day2 = do
  filePath <- getDataFileName "data/day2.txt"
  inFile <- readFile filePath
  let games = lines inFile
  let parsedGames = parseGames Map.empty games
  let validatedGames = Map.map (isGameValid gameOneBag) parsedGames

  putStrLn "Day 2"
  putStr "Part 1: "
  print (sumValidGameIds validatedGames)

  -- PART 2
  let minBags = Map.map getMinBagForGame parsedGames
  let powers = Map.map getPowerForBag minBags
  putStr "Part 2: "
  print (Map.foldr (+) 0 powers)
