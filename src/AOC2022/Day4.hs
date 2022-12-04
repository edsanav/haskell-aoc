module AOC2022.Day4 (run) where

import Utils (formatResults)
import qualified Data.Set as S
import Data.List.Split (splitOn)
import Text.Read (readMaybe)

-- sequence changes from [Maybe a] to Maybe [a]
-- sequence.map changes from (a->b) [Maybe a] to Maybe [b]
-- sequence.map is the same as traverse

parseIntervals::String -> Maybe [[Int]]
parseIntervals = traverse (toInts . splitOn "-") . splitOn ","
  where toInts xs = traverse (readMaybe ::String -> Maybe Int) xs

intervalToSet::[[Int]] -> Maybe (S.Set Int, S.Set Int)
intervalToSet ((x1:x2:_):(y1:y2:_):_) = Just (S.fromList [x1..x2], S.fromList [y1..y2])
intervalToSet _ = Nothing

isFullyContained::(S.Set Int, S.Set Int) -> Bool
isFullyContained tup = (uncurry S.isSubsetOf tup) || (uncurry.flip) S.isSubsetOf tup

doOverlap::(S.Set Int, S.Set Int) -> Bool
doOverlap tup = uncurry S.intersection tup /= S.empty

ex1:: String -> Maybe Int
ex1 = fmap (length . filter isFullyContained) . traverse  (\line -> parseIntervals line >>= intervalToSet) . lines

ex2:: String -> Maybe Int
ex2 = fmap (length . filter doOverlap) . traverse  (\line -> parseIntervals line >>= intervalToSet) . lines

run :: String -> IO()
run x = putStr $ formatResults (ex1 x) (ex2 x)