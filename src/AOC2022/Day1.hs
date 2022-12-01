module AOC2022.Day1 (run) where

import Utils (formatResults, splitStr)
import Data.List (sortOn)
import Data.Ord


sumCaloriesBlock::String -> Int
sumCaloriesBlock = sum . map read . lines 

ex1::String -> Int
ex1 =  (maximum . map sumCaloriesBlock) . splitStr "\n\n" 

ex2::String -> Int
ex2 = (sum . take 3 . sortOn Down . map sumCaloriesBlock) . splitStr "\n\n" 
  
run :: String -> IO()
run x = putStr $ formatResults (ex1 x) (ex2 x)