module Day3
  ( day3,
  )
where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List as List
import Data.Char (isDigit)
import Paths_aoc_v2023

type Coord = (Int, Int)
type Nums = [(Int, [Coord])]
type Symbols = [(Char, [Coord])]
type Gears = Map Coord [Int]

adjacentPattern :: [Coord]
adjacentPattern = [(-1,-1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1)]

getAdjacentCoordinates :: Coord -> [Coord]
getAdjacentCoordinates (x, y) = map (\(curX, curY) -> (curX + x, curY + y)) adjacentPattern

getAllSymbolAdjacentCoordinates:: Symbols -> Set Coord
getAllSymbolAdjacentCoordinates = foldr (\(_, coords) acc -> foldr Set.insert acc coords) Set.empty

parseLines :: (Nums, Symbols) -> Int -> [[Char]] -> (Nums, Symbols)
parseLines accs y (cur:rest) = parseLines newAccs (y + 1) rest
  where
    newAccs = parseLine ("", []) (0, y) cur accs
parseLines accs _ [] = accs

parseLine :: ([Char], [Coord]) -> Coord -> [Char] -> (Nums, Symbols) -> (Nums, Symbols)
parseLine ("", _) (x, y) (cur:rest) (numsAcc, symbolsAcc)
  | cur == '.' = parseLine ("", []) (x + 1, y) rest (numsAcc, symbolsAcc) -- no new digit or symbol
  | isDigit cur = parseLine ([cur], [(x, y)]) (x + 1, y) rest (numsAcc, symbolsAcc) -- new start of digit, initialize accumulator
  | otherwise = parseLine ("", []) (x + 1, y) rest (numsAcc, newSymbolsAcc) -- (acc ++ [([cur], [(x, y)])]) -- new symbol, add to return value immediately
    where
      newSymbolsAcc = symbolsAcc ++ [(cur, getAdjacentCoordinates(x, y))]
parseLine (numAcc, coordAcc) (x, y) (cur:rest) (numsAcc, symbolsAcc)
  | cur == '.' = parseLine ("", []) (x + 1, y) rest (newNumsAcc, symbolsAcc) -- end of digit, add to return value
  | isDigit cur = parseLine (numAcc ++ [cur], coordAcc ++ [(x, y)]) (x + 1, y) rest (numsAcc, symbolsAcc) -- continuation of digit, add to accumulator
  | otherwise = parseLine ("", []) (x + 1, y) rest (newNumsAcc, newSymbolsAcc) -- new symbol and end of digit, add both to return value
    where
      num = read numAcc :: Int
      newNumsAcc = numsAcc ++ [(num, coordAcc)]
      newSymbolsAcc = symbolsAcc ++ [(cur, getAdjacentCoordinates(x, y))]
parseLine ("", _) (_, _) [] acc = acc -- end of string with empty num accumulator, return
parseLine (numAcc, coordAcc) (_, _) [] (numsAcc, symbolsAcc) = (newNumsAcc, symbolsAcc) -- end of string with non-empty num accumulator, add before returning
  where
    num = read numAcc :: Int
    newNumsAcc = numsAcc ++ [(num, coordAcc)] 

isPartNumber :: Set Coord -> [Coord] -> Bool
isPartNumber adjacentCoords numCoords = case List.find (`Set.member` adjacentCoords) numCoords of
  Just _ -> True
  Nothing -> False

findPartNumbers :: Set Coord -> Nums -> Nums
findPartNumbers symbolAdjacentCoords = filter (\(_, coords) -> isPartNumber symbolAdjacentCoords coords)

-- Part 2 --

parseLines' :: (Nums, Gears) -> Int -> [[Char]] -> (Nums, Gears)
parseLines' accs y (cur:rest) = parseLines' newAccs (y + 1) rest
  where
    newAccs = parseLine' ("", []) (0, y) cur accs
parseLines' accs _ [] = accs

parseLine' :: ([Char], [Coord]) -> Coord -> [Char] -> (Nums, Gears) -> (Nums, Gears)
parseLine' ("", _) (x, y) (cur:rest) (numsAcc, gearsAcc)
  | cur == '.' = parseLine' ("", []) (x + 1, y) rest (numsAcc, gearsAcc) -- no new digit or symbol
  | isDigit cur = parseLine' ([cur], [(x, y)]) (x + 1, y) rest (numsAcc, gearsAcc) -- new start of digit, initialize accumulator
  | otherwise = parseLine' ("", []) (x + 1, y) rest (numsAcc, newGearsAcc) -- (acc ++ [([cur], [(x, y)])]) -- new symbol, add to return value immediately
    where
      newGearsAcc = Map.insert (x, y) [] gearsAcc
parseLine' (numAcc, coordAcc) (x, y) (cur:rest) (numsAcc, gearsAcc)
  | cur == '.' = parseLine' ("", []) (x + 1, y) rest (newNumsAcc, gearsAcc) -- end of digit, add to return value
  | isDigit cur = parseLine' (numAcc ++ [cur], coordAcc ++ [(x, y)]) (x + 1, y) rest (numsAcc, gearsAcc) -- continuation of digit, add to accumulator
  | otherwise = parseLine' ("", []) (x + 1, y) rest (newNumsAcc, newGearsAcc) -- new symbol and end of digit, add both to return value
    where
      num = read numAcc :: Int
      newNumsAcc = numsAcc ++ [(num, coordAcc)]
      newGearsAcc = Map.insert (x, y) [] gearsAcc
parseLine' ("", _) (_, _) [] acc = acc -- end of string with empty num accumulator, return
parseLine' (numAcc, coordAcc) (_, _) [] (numsAcc, symbolsAcc) = (newNumsAcc, symbolsAcc) -- end of string with non-empty num accumulator, add before returning
  where
    num = read numAcc :: Int
    newNumsAcc = numsAcc ++ [(num, coordAcc)]

getAdjacentCoordinates' :: [Coord] -> [Coord]
getAdjacentCoordinates' coords = List.nub (concatMap getAdjacentCoordinates coords)

getAdjacentGears :: [Coord] -> Gears -> [Coord]
getAdjacentGears coords gears = filter (`Map.member` gears) adjacentCoords
  where
      adjacentCoords = getAdjacentCoordinates' coords

getGearMap :: Nums -> Gears -> Gears
getGearMap ((curNum, curCoords):rest) gears = getGearMap rest (foldr (\coord acc -> Map.insertWith (++) coord [curNum] acc) gears adjacentGears)
  where
    adjacentGears = getAdjacentGears curCoords gears
getGearMap [] gears = gears

validateGears :: Gears -> Gears
validateGears = Map.filter (\val -> List.length val == 2)

day3 :: IO ()
day3 = do
  filePath <- getDataFileName "data/day3.txt"
  inFile <- readFile filePath
  let inFileSplit = lines inFile
  let (nums, symbols) = parseLines ([], []) 0 inFileSplit

  putStrLn "Day 3"
  putStr "Part 1: "
  let symbolAdjacentCoords = getAllSymbolAdjacentCoordinates symbols
  let partNumbers = findPartNumbers symbolAdjacentCoords nums
  print (foldr (\(num, _) acc -> num + acc) 0 partNumbers)

  putStr "Part 2: "
  let (nums', gears) = parseLines' ([], Map.empty) 0 inFileSplit
  let gearMap = getGearMap nums' gears
  let validGears = validateGears gearMap
  let ratios = Map.map product validGears
  print (sum (Map.elems ratios))