module Dec19
  ( run
  ) where

run :: IO ()
run = do
  input <- readFile "inputs/dec19.txt"
  print input
