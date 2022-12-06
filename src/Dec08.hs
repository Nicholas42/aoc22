module Dec08
  ( run
  ) where

run :: IO ()
run = do
  input <- readFile "inputs/dec08.txt"
  print input
