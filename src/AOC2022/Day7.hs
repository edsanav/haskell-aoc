module AOC2022.Day7 (run) where

import Data.List (isPrefixOf)
import Data.List.Split (keepDelimsL, split, whenElt)
import Utils (formatResults)

-- note "error with [IMPOSSIBLE] is just to silence pattern matching...code shohuld not go in there"

type Path = [String]

type FS = FileTree Int

data FileTree a = File String a Path | Directory String [FileTree a] Path
  deriving (Show, Eq, Ord)

instance Foldable FileTree where
  foldr f z (File _ a _) = f a z
  foldr f z (Directory _ content _) = foldr f z (extractContent content)
    where
      extractContent [] = []
      extractContent (File _ a _ : xs) = a : extractContent xs
      extractContent (Directory _ xxs _ : xs) = extractContent xxs ++ extractContent xs

readItem :: Path -> String -> FS
readItem path l = case words l of
  ["dir", s] -> Directory s [] path
  [sizeStr, s] -> File s (read sizeStr) path
  _ -> error $ "[IMPOSSIBLE] Invalid input line " ++ l

insert :: Path -> [String] -> FS -> FS
insert currentPath linesBlock (Directory name content path)
  | currentPath == (name : path) = Directory name (map (readItem (name : path)) linesBlock) path
  | otherwise = Directory name (map (insert currentPath linesBlock) content) path
insert _ _ fs = fs

readBlock :: [String] -> ([String], [String])
readBlock x = case split (keepDelimsL . whenElt $isPrefixOf "$") x of
  (block : remaining) -> (block, concat remaining)
  [] -> ([], [])

changeDirectory :: Path -> String -> Path
changeDirectory _ "/" = ["/"]
changeDirectory (_ : xs) ".." = xs
changeDirectory xs dirName = dirName : xs

buildFS :: Path -> [String] -> FS -> FS
buildFS path (x : remaining) fs = case (words x) of
  "$" : "cd" : dirName : _ -> buildFS (changeDirectory path dirName) remaining fs
  "$" : "ls" : _ -> buildFS path newRemaining $ insert path dirBlock fs
    where
      (dirBlock, newRemaining) = readBlock remaining
  _ -> error $ "[IMPOSSIBLE] Not command when expecting command: " ++ x
buildFS _ [] fs = fs

sumContents :: [FS] -> [Int]
sumContents [] = []
sumContents (File _ _ _ : xs) = sumContents xs
sumContents (dir@(Directory _ xxs _) : xs) = [foldr (+) 0 dir] ++ sumContents xxs ++ sumContents xs

ex1 :: String -> Int
ex1 x = sum . filter (< 100000) $ sumContents [fsTree]
  where
    fsTree = buildFS [] (lines x) (Directory "/" [] [])

ex2 :: String -> Int
ex2 x = minimum . filter (> toClear) $ sumContents [fsTree]
  where
    fsTree = buildFS [] (lines x) (Directory "/" [] [])
    totalUsed = foldr (+) 0 fsTree
    available = 70000000 - totalUsed
    toClear = 30000000 - available

run :: String -> IO ()
run x = putStr $ formatResults (ex1 x) (ex2 x)
