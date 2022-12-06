module Dec04
  ( run
  ) where

run :: IO ()
run = do
  input <- readFile "inputs/dec04.txt"
  print input
