module Dec09
  ( run
  ) where

run :: IO ()
run = do
  input <- readFile "inputs/dec09.txt"
  print input
