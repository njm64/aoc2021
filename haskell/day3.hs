module Day3 where

import Util
import Data.Bits
import qualified Aoc
import qualified Debug.Trace

maxBit = 11
allBits = reverse [0..maxBit]

parseDigit :: Char -> Bool
parseDigit '0' = False
parseDigit '1' = True
parseDigit _ = error "Invalid digit"

digitsToInt :: [Bool] -> Int
digitsToInt = foldl (\n b -> if b then n * 2 + 1 else n * 2) 0

parseBinary :: String -> Int
parseBinary s = digitsToInt $ map parseDigit s

parse :: [String] -> [Int]
parse = map parseBinary
  
countBits lst i =
  let c1 = length $ filter (\x -> x `testBit` i) lst in
  let c0 = length lst - c1 in
  (c0, c1)

mostCommonBit lst i = let (c0, c1) = countBits lst i in c1 >= c0
calcGamma nums = digitsToInt $ map (mostCommonBit nums) allBits
calcEpsilon nums = digitsToInt $ map (not . mostCommonBit nums) allBits

calcOxygen nums = step nums maxBit
  where step nums i = 
          let b = mostCommonBit nums i 
          in case filter (\x -> x `testBit` i == b) nums of
            [x] -> x
            xs -> step xs (i - 1)

calcCO2 nums = step nums maxBit
  where step nums i =
          let b = not $ mostCommonBit nums i
          in case filter (\x -> x `testBit` i == b) nums of
            [x] -> x
            xs -> step xs (i - 1)

part1 nums = calcGamma nums * calcEpsilon nums
part2 nums = calcOxygen nums * calcCO2 nums
run = Aoc.run 3 parse part1 part2

