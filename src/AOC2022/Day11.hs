module AOC2022.Day11 (run) where

import Data.Char
import Data.List.Split (splitOn)
import qualified Data.Map as M
import qualified Data.Sequence as Sq
import Utils (formatResults)

type MonkeyRef = M.Map Int Monkey

data Monkey = Monkey
  { mid :: Int,
    items :: Sq.Seq Int,
    operation :: (Int -> Int),
    toMonkey :: (Int -> Int)
  }

instance Eq Monkey where
  (==) m1 m2 = mid m1 == mid m2

instance Show Monkey where
  show m = "Monkey(" ++ (show $ mid m) ++ "): " ++ (show $ items m)

parseMonkey :: String -> Monkey
parseMonkey monkeyStr = Monkey (readNum idLine) (Sq.fromList $ readNumList itemsLine) (parseOperation operationLine) parseTest  
  where
    (idLine : itemsLine : operationLine : checkLine : trueLine : falseLine : _) = lines monkeyStr
    readNum = (read :: String -> Int) . filter isDigit
    readNumList = map (read :: String -> Int) . splitOn "," . filter (\c -> isDigit c || c == ',')
    parseTest =
      let predicate b = mod b (readNum checkLine) == 0
          runCheck y = if (predicate y) then readNum trueLine else readNum falseLine
       in runCheck
    parseOperation lnStr = op
      where
        operationWords = words . tail . dropWhile (/= '=')
        parseOperator "*" = (*)
        parseOperator "/" = (div)
        parseOperator "+" = (+)
        parseOperator "-" = (-)
        parseOperator _ = error "Invalid operator"
        op x = case operationWords lnStr of
          ("old" : opStr : "old" : _) -> (parseOperator opStr) x x
          ("old" : opStr : numStr : _) -> (parseOperator opStr) x (read numStr)
          (numStr : opStr : "old" : _) -> (parseOperator opStr) (read numStr) x
          (_) -> error "Invalid operation string"

ex1 :: String -> Int
ex1 x = 1

ex2 :: String -> Int
ex2 x = 1

run :: String -> IO ()
run x = putStr $ formatResults (ex1 x) (ex2 x)
