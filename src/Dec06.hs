module Dec06
  ( run
  ) where

run :: IO ()
run = do
  input <- readFile "inputs/dec06.txt"
  print input
