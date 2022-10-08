module Day2 where

import Util
import qualified Aoc

parseCmd s = 
  let (op, n) = splitPair ' ' s in
  (op, read n)

parse = map parseCmd
  
applyCmd (pos, depth) cmd = case cmd of
  ("forward", x)    -> (pos + x, depth)
  ("down", x)       -> (pos, depth + x)
  ("up", x)         -> (pos, depth - x)
  _ -> error ("Unknown command " ++ show cmd)

part1 cmds = 
  let (pos, depth) = foldl applyCmd (0,0) cmds in
  pos * depth

applyCmd2 (pos, depth, aim) cmd = case cmd of
  ("forward", x)    -> (pos + x, depth + (aim * x), aim)
  ("down", x)       -> (pos, depth, aim + x)
  ("up", x)         -> (pos, depth, aim - x)
  _ -> error ("Unknown command " ++ show cmd)

part2 cmds =
  let (pos, depth, _) = foldl applyCmd2 (0,0,0) cmds in
  pos * depth

run = Aoc.run 2 parse part1 part2

