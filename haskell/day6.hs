module Day6 where

import Util
import qualified Aoc
import Data.List

-- Parse input into a list of fish, represented as (timer, count) tuples
parse :: [String] -> [(Int, Int)]
parse lines = [(read s, 1) | s <- splitList ',' $ head lines]

-- Count the total number of fish in a list
count :: [(Int, Int)] -> Int
count fish = sum . (map snd) $ fish

-- Count the number of fish with a given timer
countTime :: [(Int, Int)] -> Int -> Int
countTime fish t = count . filter (\(t', _) -> t' == t) $ fish

-- Group fish with the same timer value together
groupFish :: [(Int, Int)] -> [(Int, Int)]
groupFish fish = [(t, countTime fish t) | t <- [0..9]]

-- Perform one iteration on a list of fish
tick :: [(Int, Int)] -> [(Int, Int)]
tick fish = groupFish $ foldl' f [] fish
  where f acc (0, n) = (6, n) : (8, n) : acc
        f acc (t, n) = (t - 1, n) : acc
                                            
part1 fish = count $ iterate tick fish !! 80
part2 fish = count $ iterate tick fish !! 256

run = Aoc.run 6 parse part1 part2

