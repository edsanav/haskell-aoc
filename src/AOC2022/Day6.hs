module AOC2022.Day6 (run) where

import qualified Data.Set as S
import Utils (formatResults)

hasRepetitions :: (Ord a) => [a] -> Bool
hasRepetitions xs = length xs /= (length $ S.fromList xs)

startOfThing :: Int -> Int -> String -> Int
startOfThing n acc l@(_ : xs)
  | hasRepetitions (fst $ splitAt n l) = startOfThing n (acc + 1) (xs)
  | otherwise = acc + n
startOfThing _ acc _ = acc

ex1 :: String -> Int
ex1 = startOfThing 4 0

ex2 :: String -> Int
ex2 = startOfThing 14 0

run :: String -> IO ()
run x = putStr $ formatResults (ex1 x) (ex2 x)
