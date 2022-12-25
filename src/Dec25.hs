module Dec25
  ( run
  ) where

convertDigit :: Char -> Int
convertDigit d =
  case d of
    '2' -> 2
    '1' -> 1
    '0' -> 0
    '-' -> -1
    '=' -> -2

convert :: String -> Int
convert [] = 0
convert (n:umber) = convertDigit n + 5 * (convert umber)

parseLine :: String -> Int
parseLine = convert . reverse

mod5ToSnafu :: Int -> Char
mod5ToSnafu d =
  case d of
    2 -> '2'
    1 -> '1'
    0 -> '0'
    4 -> '-'
    3 -> '='

toRevSnafu :: Int -> String
toRevSnafu 0 = ""
toRevSnafu number =
  digit : (toRevSnafu ((number - (convertDigit digit)) `div` 5))
  where
    digit = mod5ToSnafu (number `mod` 5)

run :: IO ()
run = do
  input <- map parseLine <$> lines <$> readFile "inputs/dec25.txt"
  print $ reverse $ toRevSnafu $ sum input
