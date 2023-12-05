module Day5
  ( day5,
  )
where

import qualified Data.List as List
import Data.List.Split (splitOn)
import Paths_aoc_v2023 (getDataFileName)

parseSeeds :: String -> [Int]
parseSeeds s = case splitOn ": " s of
  [_, s'] -> map read s'' :: [Int]
    where
      s'' = words s'
  _ -> []

parseMap :: String -> [(Int, Int, Int)]
parseMap str = case lines str of
  (_ : vals) -> map readVals vals
  [] -> []

readVals :: String -> (Int, Int, Int)
readVals str = case words str of
  [dest, src, len] -> (dest', src', len')
    where
      dest' = read dest :: Int
      src' = read src :: Int
      len' = read len :: Int
  _ -> (0, 0, 0)

getNewVal :: Int -> [(Int, Int, Int)] -> Int
getNewVal val maps = case List.find (\(_, src, len) -> src <= val && val <= src + len - 1) maps of
  Just (dest, src, _) -> val - src + dest 
  Nothing -> val

calcSeed :: Int -> [[(Int, Int, Int)]] -> Int
calcSeed = List.foldl' getNewVal

-- Part 2
parseSeeds' :: [Int] -> [Int]
parseSeeds' (start:len:rest) = [start..(start + len - 1)] ++ parseSeeds' rest
parseSeeds' [_] = [] -- Should never happen
parseSeeds' [] = []

day5 :: IO ()
day5 = do
  filePath <- getDataFileName "data/day5.txt"
  inFile <- readFile filePath
  let (seeds : maps) = splitOn "\n\n" inFile
  let parsedSeeds = parseSeeds seeds
  let parsedMaps = map parseMap maps
  let mappedSeeds = map (`calcSeed` parsedMaps) parsedSeeds

  putStrLn "Day 5"
  putStr "Part 1: "
  print (minimum mappedSeeds)

  -- putStr "Part 2: "
  -- let expandedSeeds = parseSeeds' parsedSeeds
  -- let mappedSeeds' = map (`calcSeed` parsedMaps) expandedSeeds
  -- print (minimum mappedSeeds')