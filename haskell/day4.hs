module Day4 where

import Util
import Data.Array
import Data.List
import qualified Aoc

-- Each cell in the board is a tuple of (number, marked)
type BoardCell = (Int, Bool)

-- The board is a 2d array of cells
type Board = Array (Int, Int) BoardCell

boardSize = 5
boardRange = [1..boardSize]

parseRow :: String -> [BoardCell]
parseRow = map (\n -> (read n, False)) . words

parseBoard :: [String] -> Board
parseBoard lines =
  array ((1,1), (boardSize, boardSize)) (zip indices cells)
  where indices = [(x,y) | x <- boardRange, y <- boardRange]
        numbers = map read . concat . map words $ lines
        cells = zip numbers (repeat False)

-- Parse input into a list of numbers, and a list of boards
parse :: [String] -> ([Int],[Board])
parse lines = 
  let numbers = map read . splitList ',' $ head lines
      boards = map parseBoard . splitList "" $ drop 2 lines
  in (numbers, boards)
  
-- Mark a board with the given number, and return a new board
markBoard :: Int -> Board -> Board
markBoard n b = fmap f b
  where f (cellNum, marked) = (cellNum, marked || cellNum == n)

-- Check a board for victory conditions
checkBoard :: Board -> Bool
checkBoard b = or [checkRow i || checkCol i | i <- boardRange]
  where checkRow y = and [snd $ b ! (x,y) | x <- boardRange]
        checkCol x = and [snd $ b ! (x,y) | y <- boardRange]

-- Find all boards that get a bingo, and return a list
-- of them, with their winning numbers, in order.
findBingos :: [Int] -> [Board] -> [(Board, Int)]
findBingos [] _ = []
findBingos _ [] = []
findBingos (n:ns) boards =
  let boards' = map (markBoard n) boards in
  let (winning, remaining) = partition checkBoard boards' in
  let winning' = map (\b -> (b, n)) winning in
  winning' ++ findBingos ns remaining

-- Calculate the score for a board (i.e. the sum of all unmarked cells)
calcScore :: Board -> Int
calcScore board = sum [n | (n, marked) <- elems board, not marked]

part1 (numbers, boards) = 
  let (b, n) = head $ findBingos numbers boards in
  calcScore b * n

part2 (numbers, boards) =
  let (b, n) = last $ findBingos numbers boards in
  calcScore b * n

run = Aoc.run 4 parse part1 part2

