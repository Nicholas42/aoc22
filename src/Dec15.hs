module Dec15
  ( run
  ) where

run :: IO ()
run = do
  input <- readFile "inputs/dec15.txt"
  print input
