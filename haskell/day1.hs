module Day1 where

import Data.List
import Util
import qualified Aoc

parse = map read
  
part1 xs = length [1 | (a,b) <- zip xs (tail xs), b > a]

part2 xs = 
  zip4 xs (tail xs) (drop 2 xs) (drop 3 xs)
  |> filter (\(a,b,c,d) -> b + c + d > a + b + c)
  |> length

run = Aoc.run 1 parse part1 part2

