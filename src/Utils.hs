module Utils (formatResults, splitEveryN, toTuple, genList, indexRows, getMatch) where

import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()
-- https://stackoverflow.com/questions/3232074/what-is-the-best-way-to-convert-string-to-bytestring

formatResults::(Show a, Show b) => a -> b -> String
formatResults x y = "Exercise 1: "++(show x)++"\n"++"Exercise 2: " ++(show y)++ "\n"

splitEveryN::Int -> [a] -> [[a]]
splitEveryN n x = case splitAt n x of
  (_, []) -> [x]
  (xs, ys) -> xs:(splitEveryN n ys)

toTuple::[a] -> (a,a)
toTuple (x:y:_) = (x,y)
toTuple _ = error $ "Trying to convert a list shorter than 2 to a tuple"

indexRows::String -> [(Int, String)]
indexRows  = zip [0 :: Int ..] . lines

genList::Int->Int->[Int]
genList x1 x2 = if (x1<=x2)
                 then [x1..x2]
                 else [x1,x1-1..x2]

getMatches :: String -> String -> [String]
getMatches input pattern = matchList
  where
    (_, _, _, matchList) = (input =~ pattern) :: (String, String, String, [String])

getMatch :: String -> String -> String
getMatch pattern input = head $ getMatches pattern input