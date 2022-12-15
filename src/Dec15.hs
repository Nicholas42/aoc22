{-# LANGUAGE OverloadedStrings #-}

module Dec15
  ( run
  ) where

import Control.Applicative ((*>), (<*), liftA2)
import qualified Data.Attoparsec.Combinator as AP
import qualified Data.Attoparsec.Text as AP
import Data.List (nub, sort)
import qualified Data.Text as T

data Position =
  Position
    { x :: Int
    , y :: Int
    }
  deriving (Show, Eq, Ord)

data Pair =
  Pair
    { sensor :: Position
    , beacon :: Position
    }
  deriving (Show, Eq, Ord)

data Range =
  Range
    { low :: Int
    , high :: Int
    }
  deriving (Show, Eq, Ord)

row = 2000000

searchLimit = 4000000

searchRange = Range 0 searchLimit

positionParser :: AP.Parser Position
positionParser =
  liftA2 Position (AP.string "x=" *> integer) $ AP.string ", y=" *> integer
  where
    integer = AP.signed AP.decimal

pairParser :: AP.Parser Pair
pairParser =
  liftA2 Pair (AP.string "Sensor at " *> positionParser) $
  AP.string ": closest beacon is at " *> positionParser <* AP.endOfLine

readPairs :: T.Text -> [Pair]
readPairs text =
  case AP.parseOnly ((AP.many' pairParser) <* AP.endOfInput) text of
    Left a -> error a
    Right b -> b

manhattan :: Position -> Position -> Int
manhattan (Position x1 y1) (Position x2 y2) = abs (x1 - x2) + abs (y1 - y2)

excluded :: Int -> Pair -> Range
excluded onRow (Pair s@(Position x y) b) = Range (x - dist) (x + dist)
  where
    dist = (manhattan s b) - abs (y - onRow)

union :: Range -> Range -> Range
union (Range l1 h1) (Range l2 h2) = Range (min l1 l2) (max h1 h2)

normalizeSortedRanges :: [Range] -> [Range]
normalizeSortedRanges [] = []
normalizeSortedRanges [x] = [x]
normalizeSortedRanges (r:a:nges)
  | low a > high r = r : (normalizeSortedRanges (a : nges))
  | otherwise = normalizeSortedRanges ((union r a) : nges)

normalizeRanges :: [Range] -> [Range]
normalizeRanges = normalizeSortedRanges . sort

aggregateExcluded :: Int -> [Pair] -> [Range]
aggregateExcluded onRow input = normalizeRanges $ map (excluded onRow) input

beaconsOnRow :: Int -> [Pair] -> [Int]
beaconsOnRow onRow list = nub $ filter (== onRow) $ map (y . beacon) list

size :: Range -> Int
size (Range l h) = h - l + 1

isEmpty :: Range -> Bool
isEmpty r = (low r) > (high r)

difference :: Range -> [Range] -> [Range]
difference parent [] = [parent]
difference parent@(Range plow phigh) ((Range clow chigh):hildren)
  | isEmpty parent = []
  | chigh < plow = difference parent hildren
  | clow < plow = difference (Range (chigh + 1) phigh) hildren
  | otherwise =
    (Range plow (clow - 1)) : (difference (Range (chigh + 1) phigh) hildren)

findPositions :: [Pair] -> Int -> [(Int, Range)]
findPositions pairs onRow =
  map ((,) onRow) $
  filter (not . isEmpty) $
  difference searchRange $ aggregateExcluded onRow pairs

findDistress :: [Pair] -> Position
findDistress pairs
  | l == h = Position l y
  | otherwise = error "Not unique"
  where
    [[(y, Range l h)]] =
      filter (not . null) $ map (findPositions pairs) [0 .. searchLimit]

calculateValue :: Position -> Int
calculateValue (Position x y) = x * searchLimit + y

run :: IO ()
run = do
  input <- readPairs <$> T.pack <$> readFile "inputs/dec15.txt"
  print $
    (sum $ map size $ aggregateExcluded row input) -
    (length $ beaconsOnRow row input)
  print $ calculateValue $ findDistress input
