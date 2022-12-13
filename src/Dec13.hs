{-# LANGUAGE OverloadedStrings #-}

module Dec13
  ( run
  ) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BS
import Data.List (sortBy, findIndices)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Vector as V

index :: [a] -> [(Int, a)]
index = zip [1 ..]

readInput :: [BS.ByteString] -> [(A.Value, A.Value)]
readInput [] = []
readInput ("":rest) = readInput rest
readInput (l:i:sts) = (readLine l, readLine i) : (readInput sts)
  where
    readLine x = fromJust (A.decode x :: Maybe A.Value)

compareL :: A.Array -> A.Array -> Ordering
compareL left right
  | null left && null right = EQ
  | null left = LT
  | null right = GT
  | result == EQ = compareL (V.tail left) (V.tail right)
  | otherwise = result
  where
    result = compareV (V.head left) (V.head right)

compareV :: A.Value -> A.Value -> Ordering
compareV (A.Number x) (A.Number y) = compare x y
compareV (A.Array x) (A.Array y) = compareL x y
compareV (A.Array x) y@(A.Number _) = compareL x (V.singleton y)
compareV x@(A.Number _) (A.Array y) = compareL (V.singleton x) y

compareP :: (A.Value, A.Value) -> Ordering
compareP = uncurry compareV

computeResult :: (Int, (A.Value, A.Value)) -> Int
computeResult (i, p) =
  case (compareP p) of
    LT -> i
    GT -> 0
    EQ -> error "No equality expected"

divider1 = fromJust $ A.decode "[[2]]" :: A.Value

divider2 = fromJust $ A.decode "[[6]]" :: A.Value

joinList :: [(A.Value, A.Value)] -> [A.Value]
joinList [] = [divider1, divider2]
joinList ((x, y):rest) = x : y : (joinList rest)

value :: [A.Value] -> Int
value list =  product $ map (1+) $ findIndices (\x -> x == divider1 || x== divider2) list

run :: IO ()
run = do
  input <- readInput <$> BS.split 10 <$> BS.readFile "inputs/dec13.txt"
  print $ sum $ map computeResult $ index input
  let sorted = sortBy compareV $ joinList input
  print $ value sorted
