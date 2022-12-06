module Dec24
  ( run
  ) where

run :: IO ()
run = do
  input <- readFile "inputs/dec24.txt"
  print input
