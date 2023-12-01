module AOC2023.Day1 (run) where

import Data.Char
import Data.Maybe
import Data.List (isPrefixOf)
import Utils (formatResults)

onlyDigits :: String -> [Int]
onlyDigits = map digitToInt . filter isDigit

digitsAndWords :: String -> [Int]
digitsAndWords [] = []
digitsAndWords ls@(x:xs)
 | isDigit x = digitToInt x:digitsAndWords xs
 | "one" `isPrefixOf` ls = 1:digitsAndWords xs
 | "two" `isPrefixOf` ls = 2:digitsAndWords xs
 | "three" `isPrefixOf` ls = 3:digitsAndWords xs
 | "four" `isPrefixOf` ls = 4:digitsAndWords xs
 | "five" `isPrefixOf` ls = 5:digitsAndWords xs
 | "six" `isPrefixOf` ls = 6:digitsAndWords xs
 | "seven" `isPrefixOf` ls = 7:digitsAndWords xs
 | "eight" `isPrefixOf` ls = 8:digitsAndWords xs
 | "nine" `isPrefixOf` ls = 9:digitsAndWords xs
 | otherwise = digitsAndWords xs


firstAndLast :: [Int] -> Maybe Int
firstAndLast ls = case ls of
  [] -> Nothing
  xs -> Just (read $ map intToDigit [head xs, last xs])

compute :: (String -> [Int]) -> String -> Int
compute parseFn input = sum $ fromJust $ traverse (firstAndLast . parseFn) (lines input)

ex1 :: String -> Int
ex1 = compute onlyDigits

ex2 :: String -> Int
ex2 = compute digitsAndWords

run :: String -> IO ()
run x = putStr $ formatResults (ex1 x) (ex2 x)
