module Day1
  ( day1
  ) where

  import Paths_aoc_v2023
  import qualified Data.Maybe as Maybe
  import qualified Data.List as List
  import qualified Data.Char as Char
  import Data.Map (Map)
  import qualified Data.Map as Map

  numStrings :: Map [Char] Char
  numStrings = Map.fromList [("one", '1'), ("two", '2'), ("three", '3'), ("four", '4'), ("five", '5'), ("six", '6'), ("seven", '7'), ("eight", '8'), ("nine", '9')]
  
  keys :: [[Char]]
  keys = Map.keys numStrings

  convertNumStrings :: [Char] -> [Char]
  convertNumStrings a@(first:rest) = case List.find (`List.isPrefixOf` a) keys of Nothing -> first : convertNumStrings rest
                                                                                  Just key -> val : convertNumStrings (last key : newRest)
                                                                                        where
                                                                                          val = numStrings Map.! key
                                                                                          newRest = Maybe.fromMaybe a (List.stripPrefix key a)
  convertNumStrings [] = []

  filterLetters :: [Char] -> [Char]
  filterLetters = filter Char.isDigit

  calibrate :: [Char] -> [Char]
  calibrate (first:rest)
    | null rest = [first, first]
    | otherwise = [first, last rest]
  calibrate [] = ['0']

  day1 :: IO()
  day1 = do
    filePath <- getDataFileName "data/day1.txt"
    inFile <- readFile filePath
    let inFileSplit = lines inFile

    -- PART 1
    let nums = map filterLetters inFileSplit
    let calibrated = map calibrate nums
    let ints = map read calibrated :: [Int]
    putStrLn "Day 1"
    putStr "Part 1: "
    print (sum ints)

    -- PART 2
    putStr "Part 2: "
    let convertedNumStrings = map convertNumStrings inFileSplit
    let nums' = map filterLetters convertedNumStrings
    let calibrated' = map calibrate nums'
    let ints' = map read calibrated' :: [Int]
    print (sum ints')
