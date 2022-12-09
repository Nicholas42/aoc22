module Dec04
  ( run
  ) where

data Range =
  Range Int Int
  deriving (Show)

data ElvePair =
  ElvePair Range Range
  deriving (Show)

readRange :: String -> (Range, String)
readRange line = do 
  let (a, as) = head $ reads line
  let (b, bs) = head $ reads $ tail as
  (Range a b, bs)
    
readPair :: String -> ElvePair
readPair line = do
  let (arange, rest) = readRange line
  let (brange, _) = readRange $ tail rest
  ElvePair arange brange

readInput :: IO [ElvePair]
readInput = map readPair <$> lines <$> readFile "inputs/dec04.txt"

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
