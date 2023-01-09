module AOC2022.Day14 (run) where

import Data.List (minimumBy, maximumBy)
import Data.List.Split (splitOn)
import qualified Data.Set as S
import Utils (formatResults, genList, toTuple)

type Coord = (Int, Int)

parseRockblock :: String -> S.Set Coord
parseRockblock = parseSection . splitOn "->"
  where
    parseSection [] = S.empty
    parseSection [_] = S.empty
    parseSection (x1 : x2 : xs) = S.union (drawRockLine (getCoords x1) (getCoords x2)) (parseSection (x2 : xs))
    getCoords = toTuple . map (read :: String -> Int) . splitOn ","
    drawRockLine (x1, y1) (x2, y2) = S.fromList [(x, y) | x <- genList x1 x2, y <- genList y1 y2]


nextObstacleInColumn :: Coord -> S.Set Coord -> Maybe Coord
nextObstacleInColumn (x0, y0) grid = nextFromTop $ S.filter (\(x, y) -> x == x0 && y >= y0) grid
  where
    nextFromTop coords
      | S.null coords = Nothing
      | otherwise = Just $ minimumBy (\(_, y1) (_, y2) -> compare y1 y2) $ S.toList coords

currentAndNextPosition :: Coord -> S.Set Coord -> (Coord, Maybe Coord)
currentAndNextPosition c grid = case nextObstacleInColumn c grid of
  Nothing -> (c, Nothing)
  Just (x1,y1) | not $ S.member (x1-1,y1)  grid -> currentAndNextPosition (x1-1, y1) grid -- left side free
  Just (x1,y1) | not $ S.member (x1+1,y1)  grid -> currentAndNextPosition (x1+1, y1) grid -- right side free
  Just c1 | c == c1 -> error "THIS SHOULD NOT HAPPEN"
  Just (x1,y1) -> (c, Just (x1, y1-1))

dropSand:: Coord -> S.Set Coord -> Int
dropSand orig grid = go grid 0
  where
    go currentGrid acc = case currentAndNextPosition orig currentGrid of
      (_, Nothing) -> acc
      (_, Just newPoint) -> go (S.insert newPoint currentGrid) acc + 1

dropSandWithFloor:: Int -> Coord -> S.Set Coord -> Int
dropSandWithFloor yFloor orig grid = go grid 0
  where
    go currentGrid acc = case currentAndNextPosition orig currentGrid of
      (_, Just (500,0))  -> acc + 1
      ((currentX, _), Nothing) -> go (S.insert (currentX, yFloor) currentGrid) acc -- Set the floor for that column
      (_, Just newPoint) -> go (S.insert newPoint currentGrid) acc + 1


ex1 :: String -> Int
ex1 x = dropSand (500,0) grid
  where grid = S.unions . map parseRockblock $ lines x

ex2 :: String -> Int
ex2 x = dropSandWithFloor yFloor (500,0) grid
  where grid = S.unions . map parseRockblock $ lines x
        yFloor = (+2).snd.maximumBy (\(_, y1) (_, y2) -> compare y1 y2) $ S.toList grid


run :: String -> IO ()
run x = putStr $ formatResults (ex1 x) (ex2 x)