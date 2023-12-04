module AOC2023.Day3 (run) where

import Utils (formatResults, indexRows)
import qualified Data.Map as M
import Data.List(sort)
import Data.Char

data Position = Position {val::String, coords::[(Int,Int)]} deriving (Show)

instance Eq Position where
  p1 == p2 = sort (coords p1) == sort (coords p2)

type Ref = M.Map (Int,Int) Char

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
adjacentSymbol ref cs = any (\x -> not (isDigit x) && (x /='.')) $ getNeighbourVals ref cs

finishNum::Ref -> Position -> Position 
finishNum ref pos@(Position posStr cs) 
  | nextIsDigit = finishNum ref (Position (posStr ++ [nextRowVal]) (cs ++ [nextRowPos]) )
  | otherwise = pos   
  where (lastRow, lastCol) = last cs
        nextRowPos = (lastRow, lastCol + 1)
        nextIsDigit = M.member (lastRow, lastCol + 1 ) ref  && isDigit (ref M.! (lastRow, lastCol + 1))
        nextRowVal = ref M.! nextRowPos

findNums::Ref -> [Position]
findNums ref = [finishNum ref (Position [ch] [cords]) | (cords@(row, col), ch) <- M.toList ref,
                                                        isDigit ch,
                                                        M.notMember (row, col-1) ref || not (isDigit (ref M.! (row, col-1)))
                                                        ]

isNearSymbol::Ref -> Position -> Bool
isNearSymbol ref pos = any (adjacentSymbol ref)  (coords pos)

-- Not the most efficient implementation at all
getAdjacentNums:: (Int,Int) -> [Position] -> [Position]
getAdjacentNums (row,col) positions = go positions []
  where neighCoords = [(i,j) | i <- [row-1..row+1], j <- [col-1..col+1]]
        isNeighbour (Position _ cs) = any (`elem` neighCoords) cs
        go [] accum = accum
        go (pos:xs) accum
          | isNeighbour pos = go xs (pos:accum)
          | otherwise = go xs accum


ex1 :: String -> Int
ex1 input = sum $ map (read.val) $ filter (isNearSymbol ref) nums
  where ref = loadRef input
        nums = findNums ref

ex2 :: String -> Int
ex2 input = sum $ map (product.map (read.val)) gearsNums
  where ref = loadRef input
        candidates = M.filter (=='*') ref
        nums = findNums ref
        gearsNums = filter ((==2).length) $ map (`getAdjacentNums` nums) (M.keys candidates)

run :: String -> IO ()
run x = putStr $ formatResults (ex1 x) (ex2 x)
