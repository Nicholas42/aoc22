module Dec04
  ( run
  ) where

data Range =
  Range Int Int
  deriving (Show)

data ElvePair =
  ElvePair Range Range
  deriving (Show)

readPair :: String -> ElvePair
readPair line = do
  let [(a, as)] = reads line
  let [(b, bs)] = reads $ tail as
  let [(c, cs)] = reads $ tail bs
  let [(d, _)] = reads $ tail cs
  ElvePair (Range a b) (Range c d)

readInput :: IO [ElvePair]
readInput = fmap ((map readPair) . lines) $ readFile "inputs/dec04.txt"

contained :: ElvePair -> Bool
contained (ElvePair (Range down up) (Range strange charm))
  | down <= strange && charm <= up = True
  | strange <= down && up <= charm = True
  | otherwise = False

overlap :: ElvePair -> Bool
overlap (ElvePair (Range down up) (Range strange char))
  | up < strange = False
  | char < down = False
  | otherwise = True

run :: IO ()
run = do
  input <- readInput
  print $ length $ filter contained input
  print $ length $ filter overlap input
