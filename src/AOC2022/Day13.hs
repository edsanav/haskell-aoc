module AOC2022.Day13(run) where

import Utils (formatResults)


import Data.Aeson
import  Data.Maybe
import qualified Data.Vector as V
import qualified Data.Scientific as Sc

import qualified Data.ByteString.Lazy.UTF8 as BLU -- from utf8-string


-- https://stackoverflow.com/questions/52218662/parse-json-rose-tree-with-haskell-aeson
-- https://stackoverflow.com/a/74324311
-- https://www.fpcomplete.com/haskell/library/aeson/

data RTree a = Leaf a | Node [RTree a] deriving (Show)


readValue::String -> Value
readValue = fromJust.decode .BLU.fromString

toTree::Value -> RTree Int
toTree (Array a) = Node $ map toTree $ V.toList a
toTree (Number n) = Leaf $ fromIntegral (Sc.coefficient n)
toTree x = error $ "Invalid JSON type" ++ show x

ex1:: String -> Int
ex1 x = 1

ex2:: String -> Int
ex2 x = 1

run :: String -> IO()
run x = putStr $ formatResults (ex1 x) (ex2 x)