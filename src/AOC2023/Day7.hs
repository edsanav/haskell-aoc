module AOC2023.Day7 (run) where

import Utils (formatResults)
import Data.List

-- Notice this implies CA < C2 !
data Card = CA | CK | CQ | CJ | CT | C9 | C8 | C7 | C6 | C5 | C4 | C3 | C2 deriving (Show, Eq, Ord)

data Hand = Five [Card] | Four [Card] | FullHouse [Card] | Three [Card] | Two [Card] | HighCard [Card] deriving (Show, Eq, Ord)

type Bid = Int

readCard :: Char -> Card
readCard c = case c of
  'A' -> CA
  'K' -> CK
  'Q' -> CQ
  'J' -> CJ
  'T' -> CT
  '9' -> C9
  '8' -> C8
  '7' -> C7
  '6' -> C6
  '5' -> C5
  '4' -> C4
  '3' -> C3
  '2' -> C2
  x -> error $ "Invalid card " ++ [x]
  
readHand:: [Card] -> Hand
readHand cards = undefined
  where grouped = group.sort $ cards
        grouped 
          | 

parseLine :: String -> (Hand, Bid)
parseLine line = undefined
  where
    (handContent, bid) = case words line of
      [handStr, bidStr] -> (map readCard handStr, read bidStr :: Int)
      _ -> error "Invalid line input"

ex1 :: String -> Int
ex1 = undefined

ex2 :: String -> Int
ex2 = undefined

run :: String -> IO ()
run x = putStr $ formatResults (ex1 x) (ex2 x)
