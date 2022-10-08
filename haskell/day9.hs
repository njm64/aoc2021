module Day9 where

import Util
import qualified Aoc
import Data.Array
import Data.Char
import Data.List
import qualified Data.Array.ST as S
import Control.Monad.ST
import Control.Monad

type Point = (Int, Int)
type HeightMap = Array Point Int

parse :: [String] -> HeightMap
parse s = array ((1,1), (w, h)) (zip indices (concat rows))
  where rows = map (map digitToInt) s
        h = length rows
        w = length (head rows)
        indices = [(x, y) | x <- [1..h], y <- [1..w]]

getHeight :: HeightMap -> Point -> Int
getHeight m p
  | inRange (bounds m) p = m ! p
  | otherwise = 9

neighbours :: Point -> [Point]
neighbours (x, y) = [(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)]

risk :: HeightMap -> Point -> Int
risk m p = if all (h <) ns then (h + 1) else 0
  where h = getHeight m p
        ns = map (getHeight m) (neighbours p)

getBasinSize m vm p = do
  if getHeight m p == 9 then return 0 else do
    visited <- S.readArray vm p
    if visited then return 0 else do
      S.writeArray vm p True
      ns <- mapM (getBasinSize m vm) (neighbours p)
      return (sum ns + 1)

getBasinSizes :: HeightMap -> [Int]
getBasinSizes m = filter (>0) $ runST $ do
  vm <- S.newArray (bounds m) False :: ST s (S.STArray s Point Bool)
  mapM (getBasinSize m vm) (indices m)

part1 input = sum [risk input i | i <- indices input]
part2 = product . take 3 . reverse . sort . getBasinSizes
run = Aoc.run 9 parse part1 part2

