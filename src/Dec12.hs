module Dec12
  ( run
  ) where

run :: IO ()
run = do
  input <- readFile "inputs/dec12.txt"
  print input
