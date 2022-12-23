{-# LANGUAGE LambdaCase #-}

module AOC2022.Day12(run) where
import qualified Data.Map as M
import Utils (formatResults)
import Debug.Trace (traceShow)
import Data.Char
import Data.Maybe

type Coord = (Int, Int)
type Node = (Coord, Char)
-- https://compprog.wordpress.com/2007/12/01/one-source-shortest-path-dijkstras-algorithm/

parseGrid::String -> M.Map Coord Node
parseGrid = M.fromList.concat.idxLineTocoords.indexLines
 where
   indexLines = zip [(0::Int)..] . lines
   idxLineTocoords = map (\(y, line) -> map (\(x, char)-> ((x,y), ((x,y), char))) $ zip [(0::Int)..] line)

getOneWithVal::Char -> M.Map Coord Char -> (Coord, Char)
getOneWithVal c = head. M.assocs . M.filter (==c)

computeH::Coord -> Coord -> Int
computeH (x1,y1) (x2, y2) = let deltaX = x2-x1
                                deltaY = y2-y1
                                in (deltaX*deltaX + deltaY*deltaY)

findNeighbours:: M.Map Coord Node -> Node -> [Node]
findNeighbours ref ((x,y), c) = map fromJust. filter isJust $ map (canMoveTo) [(x, y-1), (x, y+1), (x-1, y), (x+1, y)]
  where canMoveTo coords2 = case M.lookup coords2 ref of
                                    Just n2@(_, 'T') -> Just n2
                                    Just n2 | c=='S' -> Just n2
                                    Just n2@(_,c2) | (ord c2) - (ord c) <= 1 && c2/='S'-> Just n2
                                    _ -> Nothing
        
--buildGraph::M.Map Coord Char 

                                
                             

ex1 :: String -> Int
ex1 x = traceShow (let ref = parseGrid x in findNeighbours ref ((1,0),'a')) $ 0

ex2 :: String -> Int
ex2 x = 0

run :: String -> IO()
run x = putStr $ formatResults (ex1 x) (ex2 x)
