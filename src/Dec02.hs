module Dec02
  ( run
  ) where

run :: IO ()
run = do
  input <- readFile "inputs/dec02.txt"
  print input
