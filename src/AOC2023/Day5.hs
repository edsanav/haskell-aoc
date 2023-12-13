module AOC2023.Day5 (run) where

import Utils (formatResults)
import Data.List.Split (splitOn)

readSeeds:: String -> [Int]
readSeeds = map read.tail.words

readSeedRanges::String -> [Int]
readSeedRanges = readRange . readSeeds
  where readRange (x:y:xs) = [x..(x+y-1)] ++ readRange xs 
        readRange [] = []
        readRange [_] = error "this should not happen"

readBlock::String -> Int -> Int
readBlock input = createMapFun readNumbersInput
  where 
    toNumbersInput (dest:source:interval:_) = (dest,source,interval)
    toNumbersInput _ = error "this should not happen"
    readNumbersInput = map (toNumbersInput . map (read::String->Int) . words) $  tail.lines $ input
    createMapFun [] x = x 
    createMapFun ((dest, source, len):xs) x = if (source <= x) && x <= source+(len-1) 
                                              then dest+(x-source)
                                              else createMapFun xs x
-- y - dest + source  = x
readBlockReverse::String -> Int -> Int
readBlockReverse input = createMapFun readNumbersInput
  where
    toNumbersInput (dest:source:interval:_) = (dest,source,interval)
    toNumbersInput _ = error "this should not happen"
    readNumbersInput = map (toNumbersInput . map (read::String->Int) . words) $  tail.lines $ input
    createMapFun [] x = x 
    createMapFun ((dest, source, len):xs) y = let x = y - dest + source
                                              in if (source <= x) && x <= source+(len-1) 
                                                 then x
                                                 else createMapFun xs y


composeFinalFunction::[Int -> Int] -> Int -> Int
composeFinalFunction = foldl (flip (.)) id

ex1 :: String -> [String] -> Int
ex1 seedline blocksStr = minimum $ map finalFunc seeds
  where seeds = readSeeds seedline
        finalFunc = composeFinalFunction $ map readBlock blocksStr

ex2 :: String -> [String]  -> Int
ex2 seedline blocksStr = finalFunc 46
--ex2 seedline blocksStr = minimum $ map finalFunc seeds
  where seeds = readSeedRanges seedline
        finalFunc = composeFinalFunction $ reverse $ map readBlockReverse blocksStr


run :: String -> IO ()
run x = putStr $ formatResults (ex1 seedsline blocksStr) (ex2 seedsline blocksStr)
  where 
    (seedsline, blocksStr) = case splitOn "\n\n" x of 
      (seeds:rest) -> (seeds, rest)
      _ -> error "this should not happen: malformed input"

          
