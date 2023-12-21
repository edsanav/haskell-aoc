module AOC2023.Day7 (run) where

import Data.List
import Utils (formatResults)

-- Notice this implies CA < C2 !
data Mode = Original | WithJoker deriving (Show, Eq)

data Card = CA | CK | CQ | CJ | CT | C9 | C8 | C7 | C6 | C5 | C4 | C3 | C2 | Joker deriving (Show, Eq, Ord)

data Hand = Five [Card] | Four [Card] | FullHouse [Card] | Three [Card] | TwoPairs [Card] | OnePair [Card] | HighCard [Card] deriving (Show, Eq, Ord)

type Bid = Int

readCard :: Mode -> Char -> Card
readCard jValue c = case c of
  'A' -> CA
  'K' -> CK
  'Q' -> CQ
  'J' -> if jValue == Original then CJ else Joker
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

readHand :: [Card] -> Hand
readHand cards = hand
  where
    grouped = group . sort $ cards
    maxRepetitions = maximum . map length
    hand = case maxRepetitions grouped of
      5 -> Five cards
      4 -> Four cards
      3 | length grouped == 2 -> FullHouse cards
      3 -> Three cards
      2 | length grouped == 3 -> TwoPairs cards
      2 -> OnePair cards
      1 -> HighCard cards
      _ -> error $ "Invalid hand cards " ++ show cards

bestHand :: [Card] -> Hand
bestHand cards = transplantHand cards $ recreateHand cards
  where
    -- Probably there is a nicer way of doing this transition
    transplantHand cs (Five _) = Five cs
    transplantHand cs (Four _) = Four cs
    transplantHand cs (FullHouse _) = FullHouse cs
    transplantHand cs (Three _) = Three cs
    transplantHand cs (TwoPairs _) = TwoPairs cs
    transplantHand cs (OnePair _) = OnePair cs
    transplantHand cs (HighCard _) = HighCard cs
    --
    expandCard :: Card -> [Card]
    expandCard card = case card of
      Joker -> [CA, CK, CQ, CT, C9, C8, C7, C6, C5, C4, C3, C2]
      c -> [c]
    --
    recreateHand [a, b, c, d, e] =
      minimum $
        [ readHand [c1, c2, c3, c4, c5]
          | c1 <- expandCard a,
            c2 <- expandCard b,
            c3 <- expandCard c,
            c4 <- expandCard d,
            c5 <- expandCard e
        ]
    recreateHand _ = error "not possible"

parseLine :: Mode -> String -> (Hand, Bid)
parseLine mode line = case (mode, words line) of
  (Original, [handStr, bidStr]) -> (readHand $ map (readCard mode) handStr, read bidStr :: Int)
  (WithJoker, [handStr, bidStr]) -> (bestHand $ map (readCard mode) handStr, read bidStr :: Int)
  _ -> error "Invalid line input"

ex1 :: String -> Int
ex1 input = sum $ map (uncurry (*)) $ zipped input
  where
    zipped inp = zip [1 ..] . map snd . reverse . sort . map (parseLine Original) $ lines inp :: [(Int, Bid)]

ex2 :: String -> Int
ex2 input = sum $ map (uncurry (*)) $ zipped input
  where
    zipped inp = zip [1 ..] . map snd . reverse . sort . map (parseLine WithJoker) $ lines inp :: [(Int, Bid)]

run :: String -> IO ()
run x = putStr $ formatResults (ex1 x) (ex2 x)
