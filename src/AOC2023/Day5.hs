module AOC2023.Day5 (run) where


import Debug.Trace
import qualified Data.Map as M
import Utils (formatResults, toTuple)
import Data.List.Split (splitOn)

data Category = Seed | Soil | Fertilizer | Water | Light | Temperature | Humidity | Location deriving (Show, Eq, Ord)
data Mapping = Mapping{source::Category, dest::Category, mapping::Int -> Int}



readSeeds:: String -> [Int]
readSeeds = map read.tail.words

readBlock::String -> Mapping
readBlock input = Mapping s d (createMapFun readNumbersInput)
  where 
    (s, d) = case head $ lines input of
      "seed-to-soil map:" -> (Seed, Soil)
      "soil-to-fertilizer map:" -> (Soil, Fertilizer)
      "fertilizer-to-water map:" -> (Fertilizer, Water)
      "water-to-light map:" -> (Water, Light)
      "temperature-to-humidity map:" -> (Temperature, Humidity)
      "humidity-to-location map:" -> (Humidity, Location)
      _ -> error "unknown block"
    toNumbersInput (dest:source:interval:_) = (dest,source,interval)
    toNumbersInput _ = error "this should not happen"
    readNumbersInput = map (toNumbersInput . map (read::String->Int) . words) $  tail.lines $ input
    createMapFun [] x = x 
    createMapFun ((dest, source, len):xs) x = if (source <= x) && x <= source+(len-1) 
                                              then dest+(x-source)
                                              else createMapFun xs x


translate::  [Int] -> Mapping -> [Int]
translate input m = map (mapping m) input

ex1 :: String -> [String] -> Int
ex1 seedline blocksStr = minimum $ foldl translate seeds maps
  where seeds = readSeeds seedline
        maps = map readBlock blocksStr

ex2 :: String -> Int
ex2 = undefined

run :: String -> IO ()
run x = putStr $ formatResults (ex1 seedsline blocksStr) (ex2 x)
  where 
    (seedsline, blocksStr) = case splitOn "\n\n" x of 
      (seeds:rest) -> (seeds, rest)
      _ -> error "this should not happen: malformed input"

          
