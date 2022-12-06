module Dec22
  ( run
  ) where

run :: IO ()
run = do
  input <- readFile "inputs/dec22.txt"
  print input
