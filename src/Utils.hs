module Utils (splitStr, formatResults ) where
import qualified Data.Text as T


splitStr :: String -> String -> [String]
splitStr s x = map T.unpack $ T.splitOn (T.pack s) (T.pack x)

formatResults::(Show a, Show b) => a -> b -> String
formatResults x y = "Exercise 1: "++(show x)++"\n"++"Exercise 2: " ++(show y)++ "\n"