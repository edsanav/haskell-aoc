module Utils (formatResults, splitEveryN ) where
import qualified Data.Text as T

formatResults::(Show a, Show b) => a -> b -> String
formatResults x y = "Exercise 1: "++(show x)++"\n"++"Exercise 2: " ++(show y)++ "\n"

splitEveryN::Int -> [a] -> [[a]]
splitEveryN n x = case splitAt n x of
  (_, []) -> [x]
  (xs, ys) -> xs:(splitEveryN n ys)