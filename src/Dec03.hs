module Dec03
  ( run
  ) where

import Data.Char (ord)
import System.IO

data Rucksack =
  Rucksack String String

data Elves =
  Elves String String String

readRucksack :: String -> Rucksack
readRucksack line = do
  let halfway = div (length line) 2
  uncurry Rucksack $ splitAt halfway line

readElves :: [String] -> [Elves]
readElves (x:y:z:rest) = [Elves x y z] ++ (readElves rest)
readElves _ = []

readInput :: IO [Rucksack]
readInput = fmap ((map readRucksack) . lines) $ readFile "inputs/dec03.txt"

readInput2 :: IO [Elves]
readInput2 = fmap (readElves . lines) $ readFile "inputs/dec03.txt"

findDuplicate :: Rucksack -> Char
findDuplicate (Rucksack (a:lice) bert)
  | elem a bert = a
  | otherwise = findDuplicate (Rucksack lice bert)

priority :: Char -> Int
priority c
  | 'a' <= c && c <= 'z' = (ord c) - (ord 'a') + 1
  | 'A' <= c && c <= 'Z' = (ord c) - (ord 'A') + 27

findTriplicate :: Elves -> Char
findTriplicate (Elves (a:lice) bert carrol)
  | elem a bert && elem a carrol = a
  | otherwise = findTriplicate (Elves lice bert carrol)

run :: IO ()
run = do
  inputLines <- fmap lines $ readFile "inputs/dec03.txt"
  let input = map readRucksack inputLines
  let input2 = readElves inputLines
  print $ sum $ map (priority . findDuplicate) input
  print $ sum $ map (priority . findTriplicate) input2
