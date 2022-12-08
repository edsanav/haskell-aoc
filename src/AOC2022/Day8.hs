module AOC2022.Day8 (run) where

import qualified Data.Map as M
import Debug.Trace
import Data.Char
import Utils (formatResults)

type Row = Int

type Column = Int

type Coords = (Row, Column)

loadInput :: String -> [(Coords, Int)]
loadInput = concat . map addCol . indexRows
  where
    indexRows = zip [0 :: Int ..] . lines
    addCol (r, myLine) = map (\(c, numStr) -> ((r, c), digitToInt numStr)) $ zip [0 :: Int ..] myLine
    
blocksSight::Int -> M.Map Coords Int-> Coords -> Bool
blocksSight myH treesRef coords  = (M.findWithDefault (0) coords treesRef) >= myH

isPathClear :: (Int, Int) -> M.Map Coords Int -> (Coords, Int) -> Bool
isPathClear (maxRow, maxCol) treesRef ((row, column), h)
  | (row == 0 || column == 0 || row == maxRow || column == maxCol) = True
  | otherwise = (fromNorth || fromSouth || fromEast || fromWest)
  where
    isLineClear = not . foldr (\coords b -> (blocksSight h treesRef coords) || b) False
    fromNorth = isLineClear $ zip [0 .. (row -1)] (repeat column)
    fromSouth =  isLineClear $ zip [maxRow, (maxRow -1) .. (row + 1)] (repeat column)
    fromEast =  isLineClear $ zip (repeat row) [maxCol, (maxCol -1) .. (column + 1)]
    fromWest =  isLineClear $ zip (repeat row) [0.. (column -1)]

takeWhileOneMore :: (a -> Bool) -> [a] -> [a]
takeWhileOneMore p = foldr (\x ys -> if p x then x:ys else [x]) []

distanceTilBlocked::(Int, Int) -> M.Map Coords Int -> (Coords, Int) -> Int
distanceTilBlocked (maxRow, maxCol) treesRef ((row, column), h) = toNorth * toSouth * toEast * toWest
  where
     visionDistance x = length $ takeWhileOneMore (not . blocksSight h treesRef) x
     toNorth = visionDistance $ zip [row-1,row-2 .. 0] (repeat column)
     toSouth =  visionDistance $ zip [row+1..maxRow] (repeat column)
     toEast =  visionDistance $ zip (repeat row) [column-1,column-2 .. 0]
     toWest =  visionDistance $ zip (repeat row) [column+1 .. maxCol]



ex1 :: String -> Int
ex1 x =  length $ filter (isPathClear maxCoords treesRef) inputAsList
  where
    inputAsList = loadInput x
    maxCoords = fst $ last inputAsList
    treesRef = M.fromList inputAsList

ex2 :: String -> Int
ex2 x = maximum $ map (distanceTilBlocked maxCoords treesRef) inputAsList
  where
    inputAsList = loadInput x
    maxCoords = fst $ last inputAsList
    treesRef = M.fromList inputAsList

run :: String -> IO ()
run x = putStr $ formatResults (ex1 x) (ex2 x)
