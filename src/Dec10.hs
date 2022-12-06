module Dec10
  ( run
  ) where

run :: IO ()
run = do
  input <- readFile "inputs/dec10.txt"
  print input
