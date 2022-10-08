module Util where

import qualified Data.List as List
import qualified Data.Text as Text

(|>) x f = f x

-- Split a list exactly once at the given delimiter
splitPair :: Eq a => a -> [a] -> ([a], [a])
splitPair c s = case List.break (== c) s of
  (x, []) -> (x, [])
  (x, xs) -> (x, tail xs)

-- Split a list many times at the given delimiter 
splitList :: Eq a => a -> [a] -> [[a]]
splitList c s = case List.break (== c) s of
  (x, []) -> [x]
  (x, xs) -> x : splitList c (tail xs)

replace :: String -> String -> String -> String
replace from to = 
  let from' = Text.pack from
      to' = Text.pack to
  in Text.unpack . (Text.replace from' to') . Text.pack


