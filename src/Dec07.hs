module Dec07
  ( run
  ) where

run :: IO ()
run = do
  input <- readFile "inputs/dec07.txt"
  print input
