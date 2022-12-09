module AOC2022.Day9 (run) where

import qualified Data.Map as M
import Debug.Trace
import Data.Char
import Utils (formatResults)

type Row = Int

type Column = Int

type Coords = (Row, Column)
type H = Coords
type T = Coords

data RopeSide = Head Coords | Tail Coords deriving (Show, Ord, Eq)
data Direction = R | L | U | D deriving (Show, Ord, Eq)

parseMove:: String -> (Direction, Int)
parseMove moveStr =  case words moveStr of
  ("R":numStr:_) -> (R, read numStr)
  ("L":numStr:_) -> (L, read numStr)
  ("U":numStr:_) -> (U, read numStr)
  ("D":numStr:_) -> (D, read numStr)
  (_) -> error $ "[IMPOSSIBLE] Invalid input line "++ moveStr


moveHead:: H -> (Direction, Int) -> H
moveHead (r,c) (R,x) = (r,c+x)
moveHead (r,c) (L,x) = (r,c-x)
moveHead (r,c) (U,x) = (r+x,c)
moveHead (r,c) (D,x) = (r-x,c)

moveTail::H -> T -> T
moveTail (hRow, hCol) (tRow, tCol)
 | abs(hRow-tRow) <= 1 && abs(hCol - tCol) <= 1 = (tRow, tCol)
 | hRow == tRow = if (hCol > tCol) then (tRow, hCol-1) else (tRow, hCol+1)
 | hCol == tCol = if (hRow > tRow) then (hRow-1, tCol) else (hRow+1, tCol)

moveHead1::H -> Direction -> H
moveHead1 (r,c) (R) = (r,c+1)
moveHead1 (r,c) (L) = (r,c-1)
moveHead1 (r,c) (U) = (r+1,c)
moveHead1 (r,c) (D) = (r-1,c)

moveRope:: H -> T -> (Direction, Int) -> (H, T, [Coords])
moveRope = undefined

ex1 :: String -> Int
ex1 x =  0

ex2 :: String -> Int
ex2 x = 0

run :: String -> IO ()
run x = putStr $ formatResults (ex1 x) (ex2 x)
