{-# LANGUAGE TupleSections #-}

module AOC2021.Day4 (run) where

import Data.Function (on)
import Data.List (groupBy, intercalate, sortBy)
import qualified Data.Text as T
import System.IO


type Checkeable = [(Int, Bool)]

type Panel = [Checkeable]

sortGT :: Ord a => (a, b1) -> (a, b2) -> Ordering
sortGT (a1, _) (a2, _)
  | a1 <= a2 = LT
  | a1 > a2 = GT
  | otherwise = GT

isLine :: Checkeable -> Bool
isLine = all snd

isWinner :: Panel -> Bool
isWinner p = any isLine p || any isLine (byCols p)

byCols :: Panel -> Panel
byCols p =
  -- there's a bug here
  let withCol = [zip [0 ..] row | row <- p] :: [[(Int, (Int, Bool))]]
      -- Here on applies == to the result of applying first to the two elements it's comparing
      sorted = sortBy sortGT $ concat withCol
      grouped = groupBy ((==) `on` fst) sorted
   in [[snd x | x <- xs] | xs <- grouped]

unchecked :: Panel -> [Int]
unchecked p = map fst $ filter (not . snd) $ concat p

draw :: Int -> Panel -> Panel
draw n p =
  let mark (x, b) = if x == n then (x, True) else (x, b)
   in [map mark row | row <- p]

splitStr :: String -> String -> [String]
splitStr s x = map T.unpack $ T.splitOn (T.pack s) (T.pack x)

readMyContent :: String -> ([Int], [Panel])
readMyContent fileContent =
  let inputList = splitStr "\n\n" fileContent
      readNumsLine f inp = map (read :: String -> Int) (f inp)
      numbers = readNumsLine (splitStr ",") (head inputList)
      --      readPanel pStr = map (\x -> (x, False)) $ map readNumsLine $ lines pStr
      readPanel pStr = map (map (,False) . readNumsLine words) (lines pStr)
   in (numbers, map readPanel (tail inputList))

-- TODO multiply by last number
type Step = [Panel] -> Int -> ([Panel], Maybe Int)

firstWin :: [Panel] -> Int -> ([Panel], Maybe Int)
firstWin ps n = (nps, result $ filter isWinner nps)
  where
    nps = map (draw n) ps
    result [x] = Just (n * sum (unchecked x))
    result _ = Nothing

lastWin :: [Panel] -> Int -> ([Panel], Maybe Int)
lastWin ps n =
  if all isWinner nps
    then (nps, result $ filter (not . isWinner) ps)
    else (nps, Nothing)
  where
    nps = map (draw n) ps
    result [x] = Just (n * sum (unchecked (draw n x)))
    result _ = Nothing

loop :: [Int] -> [Panel] -> Step -> Maybe Int
loop [] _ _ = Nothing
loop (x:xs) ps step =
  case step ps x of
    (nps, Nothing) -> loop xs nps step
    (_,  res) -> res

formatPanel:: Panel -> String
formatPanel p = Data.List.intercalate "\n" (map show p)

run :: FilePath -> IO ()
run fName = do
    mydata <- readFile fName
    let inputList = splitStr "\n\n" mydata
        (instructions, panels) = readMyContent mydata
        loadedGame = loop instructions panels
--        panel1 = [ [(2,False), (0, True)],   [(14,False), (1, False)] ]
--        firstS = firstWin [panel1] 2
        result1 = loadedGame firstWin
        result2 = loadedGame lastWin
    putStrLn $ "Your data is:\nFirst result: "  ++  (show result1) ++ "\nSecond result: " ++(show result2)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
