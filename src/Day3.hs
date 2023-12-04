module Day3
  ( day3,
  )
where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List as List
import Data.Char (isDigit)
import Paths_aoc_v2023

type Coord = (Int, Int)
type Nums = [(Int, [Coord])]
type Symbols = [(Char, [Coord])]

adjacentPattern :: [Coord]
adjacentPattern = [(-1,-1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1)]

getAdjacentCoordinates :: Coord -> [Coord]
getAdjacentCoordinates (x, y) = map (\(curX, curY) -> (curX + x, curY + y)) adjacentPattern

getAllSymbolAdjacentCoordinates:: Symbols -> Set Coord
getAllSymbolAdjacentCoordinates = foldr (\(_, coords) acc -> foldr Set.insert acc coords) Set.empty

-- getGears :: Symbols -> Symbols
-- getGears = filter (\(c, _) -> c == '*')

-- getAdjacentNumbers :: Nums -> [Coord] -> [Int]
-- getAdjacentNumbers nums coords = foldr (\coord acc -> if coord `elem` coords then acc ++ [num] else acc) coords

-- getGearAdjacentNumbers :: Nums -> Symbols -> [[Int]]
-- getGearAdjacentNumbers nums gears = map (\(num, coords) -> getAdjacentNumbers nums coords) nums

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

  -- putStr "Part 2: "
  -- let gears = getGears symbols
  -- let gearAdjacentCoords = getAllSymbolAdjacentCoordinates gears
