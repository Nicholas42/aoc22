module Dec05
  ( run
  ) where

run :: IO ()
run = do
  input <- readFile "inputs/dec05.txt"
  print input
