module AOC2022.Day9 (run) where

import qualified Data.Set as S
import Utils (formatResults)

type Row = Int

type Column = Int

type Coords = (Row, Column)

type H = Coords

type T = Coords

data RopeSide = Head Coords | Tail Coords deriving (Show, Ord, Eq)

data Direction = R | L | U | D deriving (Show, Ord, Eq)

parseMove :: String -> (Direction, Int)
parseMove moveStr = case words moveStr of
  ("R" : numStr : _) -> (R, read numStr)
  ("L" : numStr : _) -> (L, read numStr)
  ("U" : numStr : _) -> (U, read numStr)
  ("D" : numStr : _) -> (D, read numStr)
  (_) -> error $ "[IMPOSSIBLE] Invalid input line " ++ moveStr

moveHead :: H -> Direction -> H
moveHead (r, c) (R) = (r, c + 1)
moveHead (r, c) (L) = (r, c -1)
moveHead (r, c) (U) = (r + 1, c)
moveHead (r, c) (D) = (r -1, c)

moveTail :: H -> T -> [Coords] -> (T, [Coords])
moveTail h@(hRow, hCol) t@(tRow, tCol) coordsList
  | distance2 h t <= 1 = ((tRow, tCol), coordsList)
  | hRow == tRow =
    if (hCol > tCol)
      then let newCoords = (tRow, tCol + 1) in (newCoords, newCoords : coordsList)
      else let newCoords = (tRow, tCol -1) in (newCoords, newCoords : coordsList)
  | hCol == tCol =
    if (hRow > tRow)
      then let newCoords = (tRow + 1, tCol) in (newCoords, newCoords : coordsList)
      else let newCoords = (tRow -1, tCol) in (newCoords, newCoords : coordsList)
  | hRow > tRow && hCol > tCol = let newCoords = (tRow + 1, tCol + 1) in (newCoords, newCoords : coordsList)
  | hRow < tRow && hCol < tCol = let newCoords = (tRow -1, tCol -1) in (newCoords, newCoords : coordsList)
  | hRow > tRow && hCol < tCol = let newCoords = (tRow + 1, tCol -1) in (newCoords, newCoords : coordsList)
  | hRow < tRow && hCol > tCol = let newCoords = (tRow -1, tCol + 1) in (newCoords, newCoords : coordsList)
  | otherwise = error $ "IMPOSSIBLE: Move tail. This should not happen"

distance2 :: (Int, Int) -> (Int, Int) -> Int
distance2 (x1, y1) (x2, y2) = floor . sqrt $ (fromIntegral (x' * x' + y' * y') :: Float)
  where
    x' = x1 - x2
    y' = y1 - y2

nextMove :: H -> T -> [Coords] -> (Direction, Int) -> ((H, T), [Coords])
nextMove h t coordsList (_, 0) = ((h, t), coordsList)
nextMove h t coordsList (d, n) = nextMove newH newT newCoordsList (d, n -1)
  where
    newH = moveHead h d
    (newT, newCoordsList) = moveTail newH t coordsList

moveRope :: H -> T -> [Coords] -> [(Direction, Int)] -> ((H, T), [Coords])
moveRope h t coordsList [] = ((h, t), coordsList)
moveRope h t coordsList (move : xs) = moveRope newH newT newCoordsList xs
  where
    ((newH, newT), newCoordsList) = nextMove h t coordsList move

ex1 :: String -> Int
ex1 = length . (S.fromList) . snd . moveRope (0, 0) (0, 0) [(0, 0)] . map parseMove . lines

ex2 :: String -> Int
ex2 x = 0

run :: String -> IO ()
run x = putStr $ formatResults (ex1 x) (ex2 x)
