module Dec06
  ( run
  ) where

hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates (r:est) = elem r est || hasDuplicates est
hasDuplicates [] = False

findDuplicateFree :: Eq a => Int -> [a] -> Int
findDuplicateFree number list
  | hasDuplicates (take number list) =
    1 + (findDuplicateFree number $ tail list)
  | otherwise = number

run :: IO ()
run = do
  input <- readFile "inputs/dec06.txt"
  print $ findDuplicateFree 4 input
  print $ findDuplicateFree 14 input
