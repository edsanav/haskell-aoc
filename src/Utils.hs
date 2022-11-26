module Utils (splitStr, ) where
import qualified Data.Text as T


splitStr :: String -> String -> [String]
splitStr s x = map T.unpack $ T.splitOn (T.pack s) (T.pack x)