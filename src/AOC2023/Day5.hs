module AOC2023.Day5 (run) where

import Data.Char
import Data.Maybe
import Data.List (isPrefixOf)
import qualified Data.Map as M
import Utils (formatResults, toTuple)
import Data.List.Split (splitOn)

data Category = Seed | Soil | Fertilizer | Water | Light | Temperature | Humidity | Location deriving (Show, Eq)
data Mapping = Mapping{source::Category, dest::Category, mapping::M.Map Int Int} deriving (Show, Eq)


readSeeds:: String -> [Int]
readSeeds = map read.tail.words

readBlock::String -> Mapping
readBlock input = undefined
  where 
    (s, d) = case head $ lines input of
      "seed-to-soil map:" -> (Seed, Soil)
      "soil-to-fertilizer map:" -> (Soil, Fertilizer)
      "fertilizer-to-water map:" -> (Fertilizer, Water)
      "water-to-light map:" -> (Water, Light)
      "temperature-to-humidity map:" -> (Temperature, Humidity)
      "humidity-to-location map:" -> (Humidity, Location)
      _ -> error "unknown block"
    readNumberLine = undefined

    
    


ex1 :: String -> [String] -> Int
ex1 seedline blocksStr = undefined

ex2 :: String -> Int
ex2 = undefined

run :: String -> IO ()
run x = putStr $ formatResults (ex1 seedsline blocksStr) (ex2 x)
  where 
    (seedsline, blocksStr) = case splitOn "\n\n" x of 
      (seeds:rest) -> (seeds, rest)
      _ -> error "this should not happen: malformed input"

          
