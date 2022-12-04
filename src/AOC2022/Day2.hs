module AOC2022.Day2 (run) where

import Utils (formatResults)


data Choice = Rock | Paper | Scissors deriving (Show, Eq)

data Result = Victory | Defeat | Draw deriving (Show, Eq)

instance Ord Choice where
  compare Scissors Rock = LT
  compare Rock Scissors = GT
  compare Rock Paper = LT
  compare Paper Rock = GT
  compare Paper Scissors = LT
  compare Scissors Paper = GT
  compare _ _ = EQ

translate::Char -> Choice
translate x
  | x == 'A' || x == 'X' = Rock
  | x == 'B' || x == 'Y' = Paper
  | x == 'C' || x == 'Z' = Scissors
  | otherwise = error "Invalid character"
  
getChoice::Choice -> Char -> Choice
getChoice Rock 'X' = Scissors
getChoice Paper 'X' = Rock
getChoice Scissors 'X' = Paper
getChoice Rock 'Z' = Paper
getChoice Paper 'Z' = Scissors
getChoice Scissors 'Z' = Rock
getChoice x _ = x 

getContenders::String -> (Choice, Choice)
getContenders [c1, _, c2] = (translate c1, translate c2)
getContenders _ = error ""

getContenders2::String -> (Choice, Choice)
getContenders2 [c1, _, c2] = (translate c1, getChoice (translate c1) c2)
getContenders2 _ = error ""


defaultScore::Choice -> Int
defaultScore Rock = 1
defaultScore Paper = 2
defaultScore Scissors = 3

getScore:: Choice -> Choice -> Int
getScore c1 c2 = case compare c1 c2 of
  GT -> 6 + defaultScore c1
  EQ -> 3 + defaultScore c1
  LT -> defaultScore c1


compute::(String -> (Choice, Choice)) -> String -> Int
compute parse = sum . map (uncurry (flip getScore). parse) . lines 

ex1:: String -> Int
ex1 = compute getContenders

ex2:: String -> Int
ex2 = compute getContenders2

run :: String -> IO()
run x = putStr $ formatResults (ex1 x) (ex2 x)