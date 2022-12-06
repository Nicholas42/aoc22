module Dec16
  ( run
  ) where

run :: IO ()
run = do
  input <- readFile "inputs/dec16.txt"
  print input
