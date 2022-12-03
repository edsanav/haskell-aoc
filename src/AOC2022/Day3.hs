module AOC2022.Day3 (run) where

import Utils (formatResults)
import Data.Char
import qualified Data.Set as S

type Group = [String]

getPriority::Char -> Int
getPriority c
 | isLower(c) = ord (c) - ord ('a') + 1
 | otherwise = ord (c) - ord ('A') + 27

tupleToList::(a,a) -> [a]
tupleToList (x,y) = [x,y]

splitInTwo::[a] -> ([a],[a])
splitInTwo xs = splitAt (div (length xs) 2) xs

splitEveryN::Int -> [a] -> [[a]]
splitEveryN n x = case splitAt n x of
  (_, []) -> [x]
  (xs, ys) -> xs:(splitEveryN n ys)

intersections::(Ord a) => [S.Set a] -> S.Set a
intersections [] = S.empty
intersections (x:xs) = foldr S.intersection x xs

computePriority::Group -> Maybe Int
computePriority = fmap getPriority.findCommon
  where findCommon = S.lookupMin.intersections.map S.fromList

ex1:: String -> Maybe Int
ex1 = fmap sum . sequence . map computePriority. map (tupleToList.splitInTwo) . lines

ex2:: String -> Maybe Int
ex2 = fmap sum . sequence . map computePriority .splitEveryN 3 . lines

run :: String -> IO()
run x = putStr $ formatResults (ex1 x) (ex2 x)