{-# LANGUAGE ViewPatterns #-}

module AOC2022.Day11 (run) where

import Data.Char
import Data.Foldable (toList)
import Data.List
import Data.Ord
import Data.List.Split (splitOn)
import qualified Data.Map as M
import qualified Data.Sequence as Sq
import Utils (formatResults)

type MonkeyMap = M.Map Int Monkey

data Monkey = Monkey
  { mid :: Int,
    items :: Sq.Seq Int,
    operation :: (Int -> Int),
    toMonkey :: (Int -> Int),
    inspected :: Int
  }

instance Eq Monkey where
  (==) m1 m2 = mid m1 == mid m2

instance Show Monkey where
  show m = "Monkey(" ++ (show $ mid m) ++ "): " ++ itemListStr ++ " [Inspected: " ++ (show $ inspected m) ++ "]"
    where
      itemListStr = intercalate "," $ map show $ toList (items m)

-- worryFunction flip div 3

parseMonkey :: (Int -> Int) -> String -> Monkey
parseMonkey worryFunction monkeyStr =
  Monkey
    { mid = readNum idLine,
      items = (Sq.fromList $ readNumList itemsLine),
      operation = (parseOperation operationLine),
      toMonkey = parseTest,
      inspected = 0
    }
  where
    (idLine : itemsLine : operationLine : checkLine : trueLine : falseLine : _) = lines monkeyStr
    readNum = (read :: String -> Int) . filter isDigit
    readNumList = map (read :: String -> Int) . splitOn "," . filter (\c -> isDigit c || c == ',')
    parseOperation lnStr = worryFunction . (op :: Int -> Int)
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
    parseTest =
      let check b = mod b (readNum checkLine) == 0
          runCheck y = if (check y) then readNum trueLine else readNum falseLine
       in runCheck

parseMonkeys :: (Int ->Int) -> String -> MonkeyMap
parseMonkeys worryFunction inStr = foldr insertMonkey M.empty $ splitOn ("\n\n") inStr
  where
    insertMonkey monkeyStr monkeyRef =
      let monkey = (parseMonkey worryFunction monkeyStr)
       in M.insert (mid monkey) monkey monkeyRef

runMonkey :: MonkeyMap -> Monkey -> MonkeyMap
runMonkey mRef Monkey {items = (Sq.null -> True)} = mRef
runMonkey mRef monkey@(Monkey mId (x Sq.:<| xs) op computeTarget oldInspected) = runMonkey updatedMonkeyRef monkeyAfter
  where
    itemAfterInspect = op x
    tId = (computeTarget itemAfterInspect)
    tMonkey = mRef M.! tId
    monkeyAfter = monkey {items = xs, inspected=oldInspected+1}
    tMonkeyAfter = tMonkey {items = (items tMonkey Sq.:|> itemAfterInspect)}
    updatedMonkeyRef = M.union (M.fromList [(mId, monkeyAfter), (tId, tMonkeyAfter)]) mRef

runRound :: MonkeyMap -> MonkeyMap
runRound monkeys = foldl (\mRef a -> runMonkey mRef (mRef M.! a)) monkeys [0 .. (length monkeys)-1]

runRounds:: Int -> MonkeyMap -> MonkeyMap
runRounds n monkeys = foldr (\_ b -> runRound b) monkeys [1..n]

ex1 :: String -> Int
ex1 x = first * second
  where
    monkeys = parseMonkeys (flip div 3) x
    (first:second:_) = sortOn Down . map (inspected.snd) $ M.toList (runRounds 20 monkeys)

ex2 :: String -> Int
ex2 x = first * second
  where
    monkeys = parseMonkeys (flip mod (2 * 13 * 5 * 3 * 11 * 17 * 7 * 19)) x
    (first:second:_) = sortOn Down . map (inspected.snd) $ M.toList (runRounds 10000 monkeys)

run :: String -> IO ()
run x = putStr $ formatResults (ex1 x) (ex2 x)
