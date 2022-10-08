module Aoc where

import System.IO
import System.CPUTime
import Text.Printf

getInput d parse = do
  let filename = "../input/day" ++ show d ++ ".txt"
  contents <- readFile filename
  let input = (parse . lines) contents
  return input

getTime :: IO Double
getTime = do
  t <- getCPUTime
  return ((fromInteger t) * 1e-12)

runPart :: Int -> Int -> a -> (a -> Int) -> IO ()
runPart dayNum partNum input f = do
  putStr $ printf "Day %02d Part %d: " dayNum partNum
  t1 <- getTime
  let !r = f input
  t2 <- getTime
  putStrLn $ printf "%-15d %.6fs" r (t2 - t1)

run d parse part1 part2 = do
  input <- getInput d parse
  runPart d 1 input part1
  runPart d 2 input part2


