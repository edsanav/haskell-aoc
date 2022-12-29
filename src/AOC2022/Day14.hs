module AOC2022.Day14(run) where

import Utils (formatResults, toTuple, genList)
import qualified Data.Set as S
import Data.List.Split (splitOn)
import Debug.Trace (traceShow)

type Coord = (Int, Int)


parseRockblock::String -> S.Set Coord
parseRockblock = parseSection . splitOn "->"
  where parseSection [] = S.empty
        parseSection (_:[]) = S.empty
        parseSection (x1:x2:xs) = S.union (drawRockLine (getCoords x1) (getCoords x2)) (parseSection (x2:xs))
        getCoords = toTuple . map (read::String -> Int) . splitOn ","
        drawRockLine (x1,y1) (x2,y2) = S.fromList [(x,y) | x <- (genList x1 x2), y <- genList y1 y2]

dropGrain::Int -> (S.Set Coord, Coord) -> (S.Set Coord, Coord)
dropGrain = undefined


ex1:: String -> Int
ex1 x  = traceShow (S.unions . map parseRockblock $ lines x) $ 0

ex2:: String -> Int
ex2 _ = 0

run :: String -> IO()
run x = putStr $ formatResults (ex1 x) (ex2 x)
