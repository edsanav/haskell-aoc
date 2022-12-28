{-# LANGUAGE LambdaCase #-}

module AOC2022.Day12(run) where
import qualified Data.Map as M
import Utils (formatResults)
import Debug.Trace (traceShow)
import Data.Char
import Data.Maybe
import qualified Data.Set as S

type Coord = (Int, Int)
type Node = (Coord, Char)
-- https://compprog.wordpress.com/2007/12/01/one-source-shortest-path-dijkstras-algorithm/
-- https://favtutor.com/blogs/breadth-first-search-python
-- https://stackoverflow.com/a/63143096

parseGrid::String -> M.Map Coord Node
parseGrid = M.fromList.concat.idxLineTocoords.indexLines
 where
   indexLines = zip [(0::Int)..] . lines
   idxLineTocoords = map (\(y, line) -> map (\(x, char)-> ((x,y), ((x,y), char))) $ zip [(0::Int)..] line)

getOneWithVal::Char -> M.Map Coord Node -> Node
getOneWithVal c = head. M.elems . M.filter ((==c). snd)


findNeighbours:: M.Map Coord Node -> Node -> [Node]
findNeighbours ref ((x,y), c) = map fromJust. filter isJust $ map (canMoveTo) [(x, y-1), (x, y+1), (x-1, y), (x+1, y)]
  where canMoveTo coords2 = case M.lookup coords2 ref of
                                    Just n2@(_, 'E') | (ord 'z' - ord c <= 1) -> Just n2
                                    Just n2 | c=='S' -> Just n2
                                    Just n2@(_,c2) | (ord c2) - (ord c) <= 1 && c2/='S' && c2/='E'-> Just n2
                                    _ -> Nothing

buildGraph::M.Map Coord Node -> M.Map Node [Node]
buildGraph ref = foldr (\node acc-> M.insert node (findNeighbours ref node) acc) M.empty ref

-- DFS doesn't necessarily finds the shortes path
dfs::M.Map Node [Node] -> S.Set Node -> Node -> Maybe Int
dfs _ visited (_,'E') = Just $ length visited
dfs graph visited node
 | S.member node visited = Nothing
 | otherwise =  head $ filter isJust results
    where results = map (dfs graph (S.insert node visited)) $ graph M.! node


bfs::M.Map Node [Node] -> S.Set Node -> [(Node, Int)] -> Maybe Int
bfs _ _ [] = Nothing
bfs graph visited ((node,distance):xs)
  | S.member node visited = bfs graph visited (xs)
  | snd node == 'E' = Just distance
  | otherwise = bfs graph newVisited (xs++neighs)
    where
      newVisited = S.insert node visited
      neighs = map (\n-> (n,distance+1)) $ graph M.! node



ex1 :: String -> Maybe Int
ex1 x = bfs graph S.empty [(start,0)]
  where
    grid = parseGrid x
    start = getOneWithVal 'S' grid
    graph = buildGraph grid

ex2 :: String -> Int
ex2 x = 0

run :: String -> IO()
run x = putStr $ formatResults (ex1 x) (ex2 x)
