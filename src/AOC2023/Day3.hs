module AOC2023.Day3 (run) where

import Utils (formatResults, indexRows)
import qualified Data.Map as M
import Data.List
import Data.Char

data Position = Position {val::String, coords::[(Int,Int)]} deriving (Show)

instance Eq Position where
  p1 == p2 = sort (coords p1) == sort (coords p2)

type Ref = M.Map (Int,Int) Char
type Size = (Int, Int)

loadRef::String -> Ref
loadRef input = M.fromList $ foldMap addCols  $ indexRows input
  where addCols (row, line) = zipWith (\col ch -> ((row, col), ch)) ([0..]::[Int]) line
getNeighboursCoords:: Ref -> (Int, Int) -> [(Int, Int)]
getNeighboursCoords ref (row, col) = [(i,j) | i <- [row-1..row+1],
                                        j <- [col-1..col+1],
                                        (i,j) /= (row,col),
                                         M.member (i,j) ref
                                      ]

getNeighbourVals:: Ref -> (Int, Int) -> [Char]
getNeighbourVals ref = map (ref M.!) . getNeighboursCoords ref

adjacentSymbol::Ref -> (Int, Int) -> Bool
adjacentSymbol ref coords = any (\x -> not (isDigit x) && (x /='.')) $ getNeighbourVals ref coords

nums:: String -> [(String, [Int])]
nums line = go 0 []
  where digits = M.fromList $ filter (isDigit.snd) $ zip [(0::Int)..] line
        go n accum
          | n >= length line  = reverse accum
          | M.notMember n digits = go (n+1) accum
          | otherwise = if M.member (n-1) digits
                        then case accum of
                                (numStr, indexes):xs -> go (n+1) $ (numStr++[digits M.! n], indexes ++ [n]):xs
                                [] -> error "This should not happen"
                        else go (n+1) $ ([digits M.! n], [n]):accum 



--nums:: String -> [[(Char, Int)]]
--nums line = undefined
--  where digits = M.fromList $ filter (isDigit.snd) $ zip [(0::Int)..] line
--        result = foldr []


getLimits::String -> (Int,Int)
getLimits input = (rows-1, cols-1)
  where ls = lines input
        rows = length ls
        cols = length (head ls)

ex1 :: String -> Int
ex1 = undefined

ex2 :: String -> Int
ex2 = undefined

run :: String -> IO ()
run x = putStr $ formatResults (ex1 x) (ex2 x)
