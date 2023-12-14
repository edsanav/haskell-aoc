module AOC2023.Day6 (run) where

import Data.List.Split (splitOn)
import Utils (formatResults)

-- 9 > x*(7 - x)
type Distance = Int

type MaxTime = Int

type HoldTime = Int

parseSeparated :: String -> [(MaxTime, Distance)]
parseSeparated input = case lines input of
  [timeLine, distanceLine] -> zip (readNumbers timeLine) (readNumbers distanceLine)
  _ -> error "this should not happen: malformed input"
  where
    readNumbers = map read.tail.words

parseTogether :: String -> (MaxTime, Distance)
parseTogether input = case lines input of
    [timeLine, distanceLine] -> (readNumbers timeLine, readNumbers distanceLine)
    _ -> error "this should not happen: malformed input"
    where
      readNumbers = read.foldl (<>) "".tail.words


computeDistanceReached :: MaxTime -> HoldTime -> Distance
computeDistanceReached maxT t = (maxT - t) * t

isValidHoldTime::(MaxTime,Distance) -> HoldTime -> Bool
isValidHoldTime (maxT, dist) t = computeDistanceReached maxT t > dist

countBetterTimes::(MaxTime, Distance) -> Int
countBetterTimes (maxT, distT) = length [t | t <- [1..(maxT-1)], isValidHoldTime (maxT, distT) t]

ex1 :: String -> Int
ex1  = product.map countBetterTimes.parseSeparated

ex2 :: String -> (Int,Int)
ex2 inpt = parseTogether inpt

run :: String -> IO ()
run x = putStr $ formatResults (ex1 x) (ex2 x)
