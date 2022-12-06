module Dec01
  ( run
  ) where

run :: IO ()
run = do
  input <- readFile "inputs/dec01.txt"
  print input
