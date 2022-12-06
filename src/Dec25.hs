module Dec25
  ( run
  ) where

run :: IO ()
run = do
  input <- readFile "inputs/dec25.txt"
  print input
