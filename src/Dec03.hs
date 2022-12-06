module Dec03
  ( run
  ) where

run :: IO ()
run = do
  input <- readFile "inputs/dec03.txt"
  print input
