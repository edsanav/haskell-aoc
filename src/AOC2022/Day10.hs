module AOC2022.Day10 (run) where

import Utils (splitEveryN)
import Debug.Trace

-- First field is the value, second field (AddX) is the cycles left
data Operation = Noop | AddX Int Int deriving (Show, Eq, Ord)


type State = Int

parseOperation::String -> [Operation]
parseOperation inStr = case words inStr of
  ("addx":numStr:_) -> let val = (read numStr) in [AddX val 1 , AddX val 0] 
  ("noop":_) -> [Noop]
  (_) -> error $ "[IMPOSSIBLE] Not a valid input"

runCycle:: State -> Operation -> State
runCycle register Noop = register
runCycle register (AddX _ 1) = register
runCycle register (AddX v 0) = register+v
runCycle _ _ = error "[IMPOSSILBE] Not a valid operation"

signalStrengthsAt:: [Int] -> [State] -> [Int]
signalStrengthsAt positions states =  map (\i -> i * states !! (i-1) ) positions 

paintPixel::Int -> Int -> Char
paintPixel pixelDrawn register = if isLit then '#' else '.'
  where row = div pixelDrawn 40
        spriteCenter = 40*row + register
        sprite = [spriteCenter-1, spriteCenter, spriteCenter+1]
        isLit = (elem pixelDrawn sprite)

ex1 :: String -> Int
ex1 =  sum . signalStrengthsAt [20,60,100,140,180,220] . states
  where operations = concat.map parseOperation . lines 
        states = init . scanl (runCycle) (1) . operations

ex2 :: String -> String
ex2 = concat . map (++"\n") . splitEveryN 40 . map (uncurry paintPixel) . zip [0..] . states
  where operations = concat.map parseOperation . lines 
        states = init . scanl (runCycle) (1) . operations

run :: String -> IO ()
run x = putStr $ "Exercise 1: "++(show (ex1 x))++"\n"++"Exercise 2: \n" ++(ex2 x)++ "\n"
