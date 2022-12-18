{-# LANGUAGE LambdaCase #-}

module AOC2022.Day13(run) where

import Utils (formatResults)


import Data.Aeson
import  Data.Maybe
import qualified Data.Vector as V
import qualified Data.Scientific as Sc
import Data.List.Split (splitOn)

import qualified Data.ByteString.Lazy.UTF8 as BLU -- from utf8-string


-- https://stackoverflow.com/questions/52218662/parse-json-rose-tree-with-haskell-aeson
-- https://stackoverflow.com/a/74324311
-- https://www.fpcomplete.com/haskell/library/aeson/

data RTree a = Leaf a | Node [RTree a] deriving (Show, Eq)

instance Ord a => Ord (RTree a) where
  compare (Leaf a1) (Leaf a2) = compare a1 a2
  compare (Node []) (Node (_:_)) = LT
  compare (Node []) (Node []) = EQ
  compare (Node (_:_)) (Node []) = GT
  compare (Node (x:xs)) (Node (y:ys))
    | x == y = compare (Node xs) (Node ys)
    | otherwise = compare x y
  compare (Leaf a1) node@(Node _) = compare (Node [Leaf a1]) node
  compare node@(Node _) (Leaf a2) = compare node (Node [Leaf a2])

readTree::String -> RTree Sc.Scientific
readTree = toTree.fromJust.decode .BLU.fromString
  where toTree (Array a) = Node $ map toTree $ V.toList a
        toTree (Number n) = Leaf $ n
        toTree x = error $ "Invalid JSON type" ++ show x

ex1:: String -> Int
ex1  = sum. map (fst). filter (snd) . zip [1..] . map comparePackets .  splitOn ("\n\n")
  where comparePackets = (\case (x:y:_) -> x < y ).map readTree . lines

ex2:: String -> Int
ex2 x = 1

run :: String -> IO()
run x = putStr $ formatResults (ex1 x) (ex2 x)

-- 6018