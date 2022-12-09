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

moveTail :: H -> T -> T
moveTail h@(hRow, hCol) t@(tRow, tCol)
  | distance2 h t <= 1 = (tRow, tCol)
  | hRow == tRow =
    if (hCol > tCol)
      then (tRow, tCol + 1)
      else (tRow, tCol -1)
  | hCol == tCol =
    if (hRow > tRow)
      then (tRow + 1, tCol)
      else (tRow -1, tCol)
  | hRow > tRow && hCol > tCol = (tRow + 1, tCol + 1)
  | hRow < tRow && hCol < tCol = (tRow -1, tCol -1)
  | hRow > tRow && hCol < tCol = (tRow + 1, tCol -1)
  | hRow < tRow && hCol > tCol = (tRow -1, tCol + 1)
  | otherwise = error $ "IMPOSSIBLE: Move tail. This should not happen"

moveTails :: H -> [T] -> [T]
moveTails h ts = tail $ scanl moveTail h ts

distance2 :: (Int, Int) -> (Int, Int) -> Int
distance2 (x1, y1) (x2, y2) = floor . sqrt $ (fromIntegral (x' * x' + y' * y') :: Float)
  where
    x' = x1 - x2
    y' = y1 - y2

nextMoves :: H -> [T] -> [Coords] -> (Direction, Int) -> ((H, [T]), [Coords])
nextMoves h ts coordsList (_, 0) = ((h, ts), coordsList)
nextMoves h ts coordsList@(c : _) (d, n) = nextMoves newH newTs newCoordsList (d, n -1)
  where
    newH = moveHead h d
    newTs = moveTails newH ts
    lastTail = last newTs
    newCoordsList = if (lastTail) == c then coordsList else (lastTail : coordsList)
nextMoves _ _ [] _ = error $ "IMPOSSIBLE: Coordlist can not be empty"

moveRope :: H -> [T] -> [Coords] -> [(Direction, Int)] -> ((H, [T]), [Coords])
moveRope h ts coordsList [] = ((h, ts), coordsList)
moveRope h ts coordsList (move : xs) = moveRope newH newTs newCoordsList xs
  where
    ((newH, newTs), newCoordsList) = nextMoves h ts coordsList move

ex1 :: String -> Int
ex1 = length . (S.fromList) . snd . moveRope (0, 0) [(0, 0)] [(0, 0)] . map parseMove . lines

ex2 :: String -> Int
ex2 = length . (S.fromList) . snd . moveRope (0, 0) (take 9 $ repeat (0, 0)) [(0, 0)] . map parseMove . lines

run :: String -> IO ()
run x = putStr $ formatResults (ex1 x) (ex2 x)
