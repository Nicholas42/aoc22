module Dec08
  ( run
  ) where

import Data.Char (digitToInt)
import Data.List (findIndex, transpose)
import Data.Maybe (fromMaybe)

cartesianMap :: (a -> b -> c) -> [a] -> [b] -> [c]
cartesianMap func as bs = [func a b | a <- as, b <- bs]

indices :: [a] -> [Int]
indices as = [0 .. (length as) - 1]

parseTrees :: [String] -> [[Int]]
parseTrees = map $ map digitToInt

startsMaximal :: [Int] -> Bool
startsMaximal [_] = True
startsMaximal (l:ist) = l > maximum ist

isVisibleInRow :: Int -> [Int] -> Bool
isVisibleInRow pos row =
  (startsMaximal $ reverse $ take (pos + 1) row) ||
  (startsMaximal $ drop pos row)

isVisible :: [[Int]] -> Int -> Int -> Bool
isVisible trees x y =
  (isVisibleInRow x $ trees !! y) || (isVisibleInRow y $ transpose trees !! x)

countVisible :: [Int] -> Int
countVisible (t:rees) = do
  let firstHigher = findIndex (>= t) rees
  maybe (length rees) succ firstHigher

scoreInRow :: [Int] -> Int -> Int
scoreInRow row x =
  countVisible (drop x row) * (countVisible $ reverse $ take (x + 1) row)

sceniceScore :: [[Int]] -> Int -> Int -> Int
sceniceScore trees x y = do
  let rowScore = scoreInRow (trees !! y) x
  let columnScore = scoreInRow (transpose trees !! x) y
  rowScore * columnScore

run :: IO ()
run = do
  input <- readFile "inputs/dec08.txt"
  let trees = parseTrees $ lines input
  let xs = indices trees
  let ys = indices $ head trees
  let visible = cartesianMap (isVisible trees) xs ys
  print $ length $ filter id visible
  print $ maximum $ cartesianMap (sceniceScore trees) xs ys
