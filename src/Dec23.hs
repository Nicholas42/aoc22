module Dec23
  ( run
  ) where

run :: IO ()
run = do
  input <- readFile "inputs/dec23.txt"
  print input
