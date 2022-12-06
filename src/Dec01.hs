module Dec01
  ( run
  ) where

import Data.List (sort)

sumHelper :: [String] -> Int -> [Int]
sumHelper ("":rest) accu = [accu] ++ (sumHelper rest 0)
sumHelper (head:rest) accu = sumHelper rest (accu + (read head))
sumHelper [] accu = [accu]

run :: IO ()
run = do
  content <- readFile "inputs/dec01.txt"
  let packs = sumHelper (lines content) 0
  let sorted = reverse $ sort packs
  print $ head sorted
  print $ sum $ take 3 sorted
