module Dec17
  ( run
  ) where

run :: IO ()
run = do
  input <- readFile "inputs/dec17.txt"
  print input
