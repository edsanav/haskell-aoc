module AOC2023.Day4 (run) where

import qualified Data.Map as M
import qualified Data.Set as S
import Utils (formatResults, getMatch)

data Card = Card {cardId :: Int, wNumbers :: S.Set Int, numbers :: S.Set Int, hits :: Int} deriving (Show, Eq)

data CardTree = Node Int [CardTree] deriving (Show, Eq)

parseCard :: String -> Card
parseCard input =
  let cardId = read $ getMatch input "Card (.*)\\:" :: Int
      patternToNumbers pattern = S.fromList $ map (read :: String -> Int) $ words $ getMatch input pattern
      winningNumbers = patternToNumbers "\\:(.*)\\|"
      numbers = patternToNumbers "\\|(.*)"
      hits = length $ S.intersection winningNumbers numbers
   in (Card cardId winningNumbers numbers hits)

computeScore :: Card -> Int
computeScore card = if (hits card == 0) then 0 else 2 ^ (hits card -1)

loadCards :: String -> M.Map Int Card
loadCards input = M.fromList $ map (\c -> (cardId c, c)) $ map parseCard $ lines input

makeTreeRef :: M.Map Int Card -> M.Map Int [Int]
makeTreeRef ref = M.fromList $ map (\(Card k _ _ hits) -> (k, [k + 1 .. k + hits])) $ M.elems ref

buildTree :: M.Map Int [Int] -> Int -> CardTree
buildTree ref start = Node start (map (buildTree ref) $ ref M.! start)

treeSize :: CardTree -> Int
treeSize (Node _ subtrees) = 1 + sum (map treeSize subtrees)

ex1 :: M.Map Int Card -> Int
ex1 ref = sum $ map computeScore $ M.elems ref

ex2 :: M.Map Int Card -> Int
ex2 ref = sum $ map (treeSize . (buildTree treeRef)) $ M.keys ref
  where
    treeRef = makeTreeRef ref

run :: String -> IO ()
run x = putStr $ formatResults (ex1 ref) (ex2 ref)
  where
    ref = loadCards x
