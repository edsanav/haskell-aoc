{-# LANGUAGE OverloadedStrings #-}

module AOC2023.Day2 (run) where

import qualified Data.Map as M
import Data.Text (pack, splitOn, unpack)
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()
import Utils (formatResults, getMatch)

-- https://hackage.haskell.org/package/regex-tdfa-1.2.3.2/docs/Text-Regex-TDFA.html

data Cube = Red | Green | Blue deriving (Show, Eq, Ord)

data Game = Game {gameId :: Int, sets :: [M.Map Cube Int]} deriving (Show, Eq, Ord)

readSet :: String -> M.Map Cube Int
readSet input = M.fromList [reds, blues, greens]
  where
    readColorOr0 pattern inp = if (inp =~ pattern :: Bool) then read $ getMatch inp pattern else 0
    reds = (Red, readColorOr0 "([0-9]*) red" input)
    blues = (Blue, readColorOr0 "([0-9]*) blue" input)
    greens = (Green, readColorOr0 "([0-9]*) green" input)

readGameLine :: String -> Game
readGameLine input = Game gameId (map readSet gameSets)
  where
    gameId = read $ getMatch input "Game (.*)\\:" :: Int
    setsBlock = head $ tail $ splitOn ":" (pack input)
    gameSets = map unpack $ splitOn ";" setsBlock

validGame :: Game -> Bool
validGame game = all validSet $ sets game
  where
    validSet set = (set M.! Red) <= 12 && set M.! Green <= 13 && set M.! Blue <= 14

getPower :: Game -> Int
getPower game = getMax Red * getMax Blue * getMax Green
  where
    getMax color = maximum $ map (M.! color) $ sets game

ex1 :: String -> Int
ex1 = sum . (map gameId) . (filter validGame) . (map readGameLine) . lines

ex2 :: String -> Int
ex2 = sum . (map getPower) . (map readGameLine) . lines

run :: String -> IO ()
run x = putStr $ formatResults (ex1 x) (ex2 x)
