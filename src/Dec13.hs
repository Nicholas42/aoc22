module Dec13
  ( run
  ) where

run :: IO ()
run = do
  input <- readFile "inputs/dec13.txt"
  print input
