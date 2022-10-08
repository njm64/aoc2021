module Day8 where

import Util
import qualified Aoc
import qualified Data.Map as Map
import Data.List
import Data.Char
import Data.Array
import Data.Bits

type Record = ([String], [String])

-- Parse each line into a tuple of two lists
parseLine :: String -> Record
parseLine s = (words a, words b)
  where (a, b) = splitPair '|' s

-- Parse input into a list of records
parse :: [String] -> [Record]
parse = map parseLine

-- Part 1: Given a record, count all codes with length 2, 3, 4, or 7
countUnique :: Record -> Int
countUnique (_, b) = length $ filter f b
  where f s = length s `elem` [2, 3, 4, 7]

-- Convert a code into a bitmask
makeMask :: String -> Int
makeMask = foldl (\m c -> setBit m (ord c - ord 'a')) 0

digitMap :: Map.Map Int Char
digitMap = Map.fromList (zip (map makeMask codes) ['0'..])
  where codes = ["abcefg", "cf", "acdeg", "acdfg", "bcdf", 
                 "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"]

codeToDigit :: String -> Char                 
codeToDigit code = case Map.lookup (makeMask code) digitMap of
  Just d -> d
  Nothing -> '?'

-- Decode a code using the given key
decode :: String -> String -> Char
decode key code = codeToDigit $ map f code 
  where a = listArray (0, length key - 1) key
        f c = a ! (ord c - ord 'a')

-- Find a key using the codes on the left hand side of an input line.
-- For each possible key, we try to decode the code, and then sort
-- the decoded digits. If it matches "0123456789", we have the correct key.
findKey :: [String] -> String
findKey codes = head . filter f $ permutations "abcdefg"
  where f k = sort (map (decode k) codes) == "0123456789"

decodeRecord :: Record -> Int
decodeRecord (a, b) = read $ map (decode key) b
  where key = findKey a 

part1 = sum . map countUnique 
part2 = sum . map decodeRecord
run = Aoc.run 8 parse part1 part2

