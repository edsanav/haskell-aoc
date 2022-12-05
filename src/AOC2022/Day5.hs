module AOC2022.Day5 (run) where

import Utils (formatResults)
import qualified Data.Map as M
import Text.Read (readMaybe)
import Data.Char
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)

type MoveFunction = Int -> String -> String -> (String, String)

-- TODO refactor this abomination
parseCrates::String -> M.Map Char [Char]
parseCrates inStr = foldr enterLine  M.empty (reverse cratesLines)
  where (idsLine:cratesLines) = reverse $ lines inStr
        idxRef = filter (isDigit.snd) $ zip [0..] idsLine
        addToStore (_,' ') store =  store
        addToStore (k,v) store =  M.insert k (v : (M.findWithDefault [] k store)) store
        enterLine lineStr store = foldr (\a st -> addToStore a st) store $ toInsertInput idxRef lineStr
        toInsertInput refs line = map (\(idx, k) -> (k, line !! idx)) refs

parseMove::String -> (Int, Char, Char)
parseMove  = toOutput.toList
  where toList = catMaybes. map (readMaybe ::String -> Maybe Int) . words
        toOutput (x:y:z:[]) = (x, intToDigit y, intToDigit z)

move1by1::Int -> String -> String -> (String, String)
move1by1 0 x y = (x, y)
move1by1 n (x:xs) (y) = move1by1 (n-1) (xs) (x:y)
move1by1 _ x@_ y@_ = (x,y)

moveAtOnce::Int -> String -> String -> (String, String)
moveAtOnce n x y = (remaining, toMove ++ y)
  where (toMove, remaining) = splitAt n x

move::MoveFunction -> Int -> Char -> Char -> M.Map Char [Char] -> M.Map Char [Char]
move fn n sourceK destK store = M.insert destK destAfter $ M.insert sourceK sourceAfter store
  where
    (sourceAfter, destAfter) =  fn n source dest
    source = store M.! sourceK
    dest = store M.! destK

operate::MoveFunction -> String -> String
operate moveFn x = M.foldr ((:).head) [] endState
  where (cratesStr:movesStr:_) = splitOn "\n\n" x
        initialState = parseCrates cratesStr
        moves = map parseMove $ lines movesStr
        endState = foldr (\(n,source,dest) b -> move moveFn n source dest b) initialState $ reverse moves

ex1:: String -> String
ex1 = operate move1by1


ex2:: String -> String
ex2 = operate moveAtOnce

run :: String -> IO()
run x = putStr $ formatResults (ex1 x) (ex2 x)