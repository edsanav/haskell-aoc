module AOC2023.Day1 (run) where

import Utils (formatResults)
import Data.List (sortOn)
import Data.Ord
import Data.List.Split (splitOn)
import Data.Char
import Data.Maybe

toNums::String -> [Int]
toNums = map digitToInt . filter isDigit

firstAndLast::[Int] -> Maybe Int
firstAndLast ls = case ls of 
  [] -> Nothing
  xs -> Just (read $ map intToDigit [head xs, last xs])
  
ex1::String -> Int
ex1 inp =  sum $ fromJust $ traverse (firstAndLast.toNums) (lines inp)

ex2::String -> Int
ex2 = undefined
  
run :: String -> IO()
run x = putStr $ formatResults (ex1 x) (ex2 x)