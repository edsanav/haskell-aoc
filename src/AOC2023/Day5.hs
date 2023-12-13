module AOC2023.Day5 (run) where

import Utils (formatResults)
import Data.List.Split (splitOn)

readSeeds:: String -> [Int]
readSeeds = map read.tail.words



readMappings::String -> [(Int,Int,Int)]
readMappings input = map (toNumbersInput . map (read::String->Int) . words) $  tail.lines $ input
   where
     toNumbersInput (dest:source:interval:_) = (dest,source,interval)
     toNumbersInput _ = error "this should not happen"

directFunc::[(Int,Int,Int)] -> (Int -> Int)
directFunc [] x = x
directFunc ((dest, source, len):xs) x = if (source <= x) && x <= source+(len-1) 
                                          then dest+(x-source)
                                          else directFunc xs x

-- y - dest + source  = x
reverseFunc::[(Int,Int,Int)] -> (Int -> Int)
reverseFunc [] y = y
reverseFunc ((dest, source, len):xs) y = let x = y - dest + source
                                         in if (source <= x) && x <= source+(len-1)
                                            then x
                                            else reverseFunc xs y
                                            
isValidOutput::String -> Int -> Bool
isValidOutput = readRange.readSeeds
  where readRange (start:amount:xs) x = (start <= x) && (x <= (start+amount-1)) || readRange xs x
        readRange [] _ = False
        readRange [_] _ = error "this should not happen"


composeFinalFunction::[Int -> Int] -> Int -> Int
composeFinalFunction = foldl (flip (.)) id

ex1 :: String -> [String] -> Int
ex1 seedline blocksStr = minimum $ map finalFunc seeds
  where seeds = readSeeds seedline
        finalFunc = composeFinalFunction $ map (directFunc.readMappings) blocksStr

ex2 :: String -> [String]  -> Int
ex2 seedline blocksStr = head valid
  where validOutputFunc = isValidOutput seedline
        finalFunc = composeFinalFunction $ reverse $ map (reverseFunc.readMappings) blocksStr
        (_, valid) = break (validOutputFunc.finalFunc) [0..]


run :: String -> IO ()
run x = putStr $ formatResults (ex1 seedsline blocksStr) (ex2 seedsline blocksStr)
  where 
    (seedsline, blocksStr) = case splitOn "\n\n" x of 
      (seeds:rest) -> (seeds, rest)
      _ -> error "this should not happen: malformed input"

          
