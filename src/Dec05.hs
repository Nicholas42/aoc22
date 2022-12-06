module Dec05
  ( run
  ) where

import Data.Char (isDigit, isLetter)
import Data.List (isPrefixOf, partition, transpose)

data Move =
  Move
    { number :: Int
    , from :: Int
    , to :: Int
    }
  deriving (Show)

splitInput :: String -> ([String], [String])
splitInput input = do
  let lined = filter (/= "") $ lines input
  partition (isPrefixOf "move") lined

readMove :: String -> Move
readMove input = do
  let [number, from, to] = map read $ filter (all isDigit) $ words input
  Move number (from - 1) (to - 1)

readStacks :: [String] -> [[Char]]
readStacks input = filter (/= "") $ map (filter isLetter) $ transpose input

replaceElem :: Int -> a -> [a] -> [a]
replaceElem index elem list =
  (take index list) ++ elem : (drop (index + 1) list)

applySingleMove :: Move -> [[Char]] -> [[Char]]
applySingleMove (Move _ from to) stacks = do
  let moved = head $ stacks !! from
  let movedFrom = tail $ stacks !! from
  let movedTo = moved : (stacks !! to)
  replaceElem from movedFrom $ replaceElem to movedTo stacks

applyMove :: [[Char]] -> Move -> [[Char]]
applyMove stacks move = do
  iterate (applySingleMove move) stacks !! (number move)

applyMove2 :: [[Char]] -> Move -> [[Char]]
applyMove2 stacks (Move number from to) = do
  let fromStack = stacks !! from
  let fromStackPreReversed =
        (reverse $ take number fromStack) ++ (drop number fromStack)
  let preReversed = replaceElem from fromStackPreReversed stacks
  applyMove preReversed (Move number from to)

run :: IO ()
run = do
  input <- readFile "inputs/dec05.txt"
  let (moveString, stackString) = splitInput input
  let moves = map readMove moveString
  let stacks = readStacks stackString
  let finalStacks = foldl applyMove stacks moves
  let finalStacks2 = foldl applyMove2 stacks moves
  putStrLn $ map head finalStacks
  putStrLn $ map head finalStacks2
