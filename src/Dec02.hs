module Dec02
  ( run
  ) where

data Sign
  = Rock
  | Paper
  | Scissors
  deriving (Eq, Ord, Show)

data Result
  = Lose
  | Draw
  | Win

data Round =
  Round Sign Sign

data Round2 =
  Round2 Sign Result

winner :: Sign -> Sign
winner Rock = Paper
winner Paper = Scissors
winner Scissors = Rock

loser = winner . winner

resultFunc :: Result -> Sign -> Sign
resultFunc Lose = loser
resultFunc Win = winner
resultFunc Draw = id

mapToSign :: Char -> Sign
mapToSign x =
  case x of
    'A' -> Rock
    'B' -> Paper
    'C' -> Scissors
    'X' -> Rock
    'Y' -> Paper
    'Z' -> Scissors

mapToResult :: Char -> Result
mapToResult x =
  case x of
    'X' -> Lose
    'Y' -> Draw
    'Z' -> Win

readRound :: String -> Round
readRound (x:_:y:_) = Round (mapToSign x) (mapToSign y)

readRound2 :: String -> Round
readRound2 (x:_:y:_) = do
  let sign = mapToSign x
  Round sign $ resultFunc (mapToResult y) sign

evalRounds :: [Round] -> Int
evalRounds rounds = sum $ map (\x -> (playValue x) + (evalVictor x)) rounds

evalVictor :: Round -> Int
evalVictor (Round x y)
  | x == y = 3
  | y == (winner x) = 6
  | otherwise = 0

playValue :: Round -> Int
playValue (Round _ x) =
  case x of
    Rock -> 1
    Paper -> 2
    Scissors -> 3

run :: IO ()
run = do
  inputLines <- lines <$> readFile "inputs/dec02.txt"
  let input = map readRound inputLines
  let input2 = map readRound2 inputLines
  print $ evalRounds input
  print $ evalRounds input2
