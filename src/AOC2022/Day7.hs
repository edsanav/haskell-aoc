module AOC2022.Day7 (run) where

import Utils (formatResults)
import Data.List (isPrefixOf)
import Data.List.Split (splitOn, split, keepDelimsL, whenElt)
import Debug.Trace

type Path = [String]
type Content = [FS]

data FS = File String Int Path | Directory String Content Path deriving (Show, Eq, Ord)

-- note "error with [IMPOSSIBLE] is just to silence pattern matching...code shohuld not go in there"

readItem:: Path -> String -> FS
readItem path l = case words l of
  ["dir", s] -> Directory s [] path
  [sizeStr, s] -> File s (read sizeStr) path
  _ -> error $ "[IMPOSSIBLE] Invalid input line "++l

insert::Path -> [String] -> FS -> FS
insert currentPath linesBlock (Directory name content path)
  | currentPath == (name:path) =  traceShow "MATCH2!" $ Directory name (map (readItem (name:path)) linesBlock) path
  | otherwise = Directory name (map (insert currentPath linesBlock)  content) path
insert _ _ fs = fs

readBlock::[String]->([String],[String])
readBlock x = case split (keepDelimsL.whenElt $isPrefixOf "$") x of
  (block:remaining) -> (block, concat remaining)
  [] -> ([],[])

changeDirectory::Path -> String -> Path
changeDirectory _ "/" = ["/"]
changeDirectory (_:xs) ".." =  xs
changeDirectory xs dirName = dirName:xs

buildFS::Path -> [String] -> FS -> FS
buildFS path (x:remaining) fs = case (words x) of
  "$":"cd":dirName:_ -> buildFS (changeDirectory path dirName) remaining fs
  "$":"ls":_ -> buildFS path newRemaining $ insert path dirBlock fs
    where (dirBlock, newRemaining) = readBlock remaining
  _ -> error $ "[IMPOSSIBLE] Not command when expecting command: " ++ x
buildFS _ [] fs = fs

ex1 :: String -> FS
ex1 x = buildFS [] (lines x) (Directory "/" [] [])

ex2 :: String -> [String]
ex2 x = []
--ex2 x = lines x

run :: String -> IO ()
run x = putStr $ formatResults (ex1 x) (ex2 x)


bla = Directory "/" [
          Directory "a" [
            Directory "e" [
              File "i" 584 ["e","a","/"]
            ] ["a","/"],
            File "f" 29116 ["a","/"],
            File "g" 2557 ["a","/"],
            File "h.lst" 62596 ["a","/"]] ["/"],
          File "b.txt" 14848514 ["/"],
          File "c.dat" 8504156 ["/"],
          Directory "d" [
            File "j" 4060174 ["d","/"],
            File "d.log" 8033020 ["d","/"],
            File "d.ext" 5626152 ["d","/"],
            File "k" 7214296 ["d","/"]] ["/"]
       ] ["/"]
