{-# LANGUAGE LambdaCase #-}

module AOC2022.Day12(run) where
import qualified Data.Map as M
import Utils (formatResults)
import Debug.Trace (traceShow)

type Coord = (Int, Int)
-- https://compprog.wordpress.com/2007/12/01/one-source-shortest-path-dijkstras-algorithm/

parseGrid::String -> M.Map Coord Char
parseGrid = M.fromList.concat.idxLineTocoords.indexLines
 where
   indexLines = zip [(0::Int)..] . lines
   idxLineTocoords = map (\(y, line) -> map (\(x, char)-> ((x,y), char)) $ zip [(0::Int)..] line)

getOneWithVal::Char -> M.Map Coord Char -> (Coord, Char)
getOneWithVal c = head. M.assocs . M.filter (==c)

computeH::Coord -> Coord -> Int
computeH (x1,y1) (x2, y2) = let deltaX = x2-x1
                                deltaY = y2-y1
                                in (deltaX*deltaX + deltaY*deltaY)

ex1 :: String -> Int
ex1 x = traceShow (computeH (3,7) (0,0)) $ 0

ex2 :: String -> Int
ex2 x = 0

run :: String -> IO()
run x = putStr $ formatResults (ex1 x) (ex2 x)
