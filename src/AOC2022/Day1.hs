module AOC2022.Day1 (run) where

import Utils (formatResults)
import Data.List (sortOn)
import Data.Ord
import Data.List.Split (splitOn)


sumCaloriesBlock::String -> Int
sumCaloriesBlock = sum . map read . lines 

ex1::String -> Int
ex1 =  (maximum . map sumCaloriesBlock) . splitOn "\n\n"

ex2::String -> Int
ex2 = (sum . take 3 . sortOn Down . map sumCaloriesBlock) . splitOn "\n\n"
  
run :: String -> IO()
run x = putStr $ formatResults (ex1 x) (ex2 x)