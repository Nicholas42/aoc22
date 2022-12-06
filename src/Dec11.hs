module Dec11
  ( run
  ) where

run :: IO ()
run = do
  input <- readFile "inputs/dec11.txt"
  print input
