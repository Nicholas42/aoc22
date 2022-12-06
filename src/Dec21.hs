module Dec21
  ( run
  ) where

run :: IO ()
run = do
  input <- readFile "inputs/dec21.txt"
  print input
