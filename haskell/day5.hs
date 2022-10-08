module Day5 where

import Util
import Data.Array
import qualified Aoc

mapSize = 1000

type Cmd = (Int, Int, Int, Int)
type Map = Array (Int, Int) Int

parseCmd :: String -> Cmd
parseCmd s = makeTuple . map read . splitList ',' $ replace " -> " "," s
  where makeTuple [x1, y1, x2, y2] = (x1, y1, x2, y2)
        makeTuple _ = error "Invalid command"

parse :: [String] -> [Cmd]
parse = map parseCmd
  
makeRange :: Int -> Int -> [Int]
makeRange a b 
  | a < b = [a..b]
  | a > b = [a,a-1..b]
  | otherwise = [a]

cmdToPoints :: Cmd -> [(Int, Int)]
cmdToPoints (x1, y1, x2, y2)
  | y1 == y2  = [(x, y1) | x <- makeRange x1 x2]
  | x1 == x2  = [(x1, y) | y <- makeRange y1 y2]
  | otherwise = zip (makeRange x1 x2) (makeRange y1 y2)
    
buildMap :: [Cmd] -> Map
buildMap cmds = accumArray (+) 0 ((1,1), (mapSize, mapSize)) updates
  where points = concat $ map cmdToPoints cmds 
        updates = map (\p -> (p, 1)) points

countCells :: Map -> Int
countCells = length . filter (>=2) . elems

part1 :: [Cmd] -> Int
part1 cmds = countCells . buildMap $ filter straight cmds
  where straight (x1, y1, x2, y2) = x1 == x2 || y1 == y2

part2 :: [Cmd] -> Int
part2 cmds = countCells $ buildMap cmds

run = Aoc.run 5 parse part1 part2

