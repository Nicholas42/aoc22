module Dec14
  ( run
  ) where

run :: IO ()
run = do
  input <- readFile "inputs/dec14.txt"
  print input
