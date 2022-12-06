module Dec20
  ( run
  ) where

run :: IO ()
run = do
  input <- readFile "inputs/dec20.txt"
  print input
