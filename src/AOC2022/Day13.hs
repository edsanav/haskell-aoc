module AOC2022.Day13(run) where

import Utils (formatResults)


import Data.Aeson

import qualified Data.ByteString.Lazy.UTF8 as BLU -- from utf8-string


-- https://stackoverflow.com/questions/52218662/parse-json-rose-tree-with-haskell-aeson
-- https://stackoverflow.com/a/74324311
-- https://www.fpcomplete.com/haskell/library/aeson/
--data RTree a = Leaf a | Node [RTree a] deriving Show
--
--type Packet = RTree Int
--
--
data RTree a = Leaf a | Node [RTree a] deriving Show


-- TODO: Convert value to RTree (check fromJSON to convert array to list and Number
readValue::String -> Value
readValue x  = vInJSON
  where vInJSON = case (decode $ BLU.fromString x) of 
                   Just myVal -> myVal::Value
                   Nothing -> error "Impossible"
              
bla:: Maybe a -> a
bla (Just x) = x

ex1:: String -> Int
ex1 x = 1

ex2:: String -> Int
ex2 x = 1

run :: String -> IO()
run x = putStr $ formatResults (ex1 x) (ex2 x)