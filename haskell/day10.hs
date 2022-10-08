module Day10 where

import Util
import qualified Aoc
import Data.Maybe
import Data.List

data Result = Ok | UnexpectedToken Char | UnterminatedExpr String 

parseExpr :: [Char] -> [Char] -> Result
parseExpr [] [] = Ok
parseExpr tags [] = UnterminatedExpr tags
parseExpr tags ('{' : tl) = parseExpr ('}' : tags) tl
parseExpr tags ('[' : tl) = parseExpr (']' : tags) tl
parseExpr tags ('<' : tl) = parseExpr ('>' : tags) tl
parseExpr tags ('(' : tl) = parseExpr (')' : tags) tl
parseExpr [] (hd : _) = UnexpectedToken hd
parseExpr (hd_tag : tl_tags) (hd : tl)
  | hd == hd_tag = parseExpr tl_tags tl
  | otherwise = UnexpectedToken hd

parse :: [String] -> [Result]
parse lines = map (parseExpr []) lines

middle :: [a] -> a
middle lst = lst !! (length lst `div` 2)

part1 = sum . map score 
  where score (UnexpectedToken ')') = 3
        score (UnexpectedToken ']') = 57
        score (UnexpectedToken '}') = 1197
        score (UnexpectedToken '>') = 25137
        score _ = 0

part2 = middle . sort . catMaybes . map score
  where score (UnterminatedExpr tags) = Just (foldl f 0 tags)
        score _ = Nothing
        f n ')' = n * 5 + 1
        f n ']' = n * 5 + 2
        f n '}' = n * 5 + 3
        f n '>' = n * 5 + 4

run = Aoc.run 10 parse part1 part2

