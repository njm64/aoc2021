module Day7 where

import Util
import qualified Aoc
import Data.Array

-- Parse input into a list of numbers
parse :: [String] -> [Int]
parse lines = map read . splitList ',' $ head lines

-- Build a table with the cost of travelling distance i at each index
makeFuelTable :: Int -> Array Int Int
makeFuelTable n = array (0, n) (zip [0..] costs)
  where costs = scanl (+) 0 [1..n]

-- Calculate the fuel to move all crabs to a given position
calcFuel :: [Int] -> (Int -> Int) -> Int -> Int
calcFuel crabs f pos = sum $ map (f . abs . (pos `subtract`)) crabs

-- Calculate minimum fuel using a cost function f
calcMinFuel :: [Int] -> (Int -> Int) -> Int
calcMinFuel crabs f = minimum [calcFuel crabs f pos | pos <- [0..maxPos]]
  where maxPos = maximum crabs + 1

-- Part 1: Cost is just the identity function
part1 crabs = calcMinFuel crabs id

-- Part 2: Build a lookup table with fuel costs
part2 crabs = calcMinFuel crabs (t !)
  where t = makeFuelTable (maximum crabs + 1)

run = Aoc.run 7 parse part1 part2

