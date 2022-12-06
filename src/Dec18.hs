module Dec18
  ( run
  ) where

run :: IO ()
run = do
  input <- readFile "inputs/dec18.txt"
  print input
