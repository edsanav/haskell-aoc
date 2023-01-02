module AOC2022.Day14 (run) where

import Data.List (sortBy)
import Data.List.Split (splitOn)
import qualified Data.Set as S
import Debug.Trace (traceShow)
import Utils (formatResults, genList, toTuple)

type Coord = (Int, Int)

parseRockblock :: String -> S.Set Coord
parseRockblock = parseSection . splitOn "->"
  where
    parseSection [] = S.empty
    parseSection (_ : []) = S.empty
    parseSection (x1 : x2 : xs) = S.union (drawRockLine (getCoords x1) (getCoords x2)) (parseSection (x2 : xs))
    getCoords = toTuple . map (read :: String -> Int) . splitOn ","
    drawRockLine (x1, y1) (x2, y2) = S.fromList [(x, y) | x <- (genList x1 x2), y <- genList y1 y2]


nextObstacleInColumn :: Coord -> S.Set Coord -> Maybe Coord
nextObstacleInColumn (x0, y0) grid = nextFromTop $ S.filter (\(x, y) -> x == x0 && y >= y0) grid
  where
    nextFromTop coords
      | S.null coords = Nothing
      | otherwise = Just $ head . sortBy (\(_, y1) (_, y2) -> compare y1 y2) $ S.toList coords

potentialNextPosition :: Coord -> S.Set Coord -> Maybe Coord
potentialNextPosition c grid = case (nextObstacleInColumn c) grid of
  Nothing -> Nothing
  Just (x1,y1) | not $ S.member (x1-1,y1)  grid -> potentialNextPosition (x1-1, y1) grid -- left side free
  Just (x1,y1) | not $ S.member (x1+1,y1)  grid -> potentialNextPosition (x1+1, y1) grid -- right side free
  Just c1 | c == c1 -> error "THIS SHOULD NOT HAPPEN"
  Just (x1,y1) -> Just (x1, y1-1) 
  
  
ex1 :: String -> Int
ex1 x = traceShow (potentialNextPosition (500,0) $ S.union extra grid) $ 0
  where grid = S.unions . map parseRockblock $ lines x
        extra = S.fromList [(500,8), (499,8), (501,8)]

ex2 :: String -> Int
ex2 _ = 0

run :: String -> IO ()
run x = putStr $ formatResults (ex1 x) (ex2 x)
