module Dec20
  ( run
  ) where

import qualified Data.List as L
import Debug.Trace

readInt :: String -> Int
readInt = read

insertAt :: Int -> a -> [a] -> [a]
insertAt index elem list = before ++ (elem : after)
  where
    (before, after) = L.splitAt index list

circMod :: Int -> Int -> Int
circMod modded modulus = mod modded (modulus - 1)

move :: Int -> [(Int, Int)] -> [(Int, Int)]
move index list
  | move == index = list
  | move < length prefix = (insertAt move s prefix) ++ uffix
  | otherwise = prefix ++ insertAt (move - length prefix) s uffix
  where
    (prefix, (s:uffix)) = L.splitAt index list
    move = circMod (index + snd s) $ length list

moveAll :: Int -> [(Int, Int)] -> [(Int, Int)]
moveAll number list
  | number == length list = list
  | otherwise = do
    let (Just index) = L.findIndex (== number) $ map fst list
    moveAll (1 + number) $ move index list

runMoves :: Int -> [Int] -> [Int]
runMoves times list = map snd $ (iterate (moveAll 0) $ zip [0 ..] list) !! times

decrypt :: [Int] -> [Int]
decrypt list = map (* 811589153) list

calcValue :: [Int] -> Int
calcValue list = sum $ map index [1000, 2000, 3000]
  where
    Just initial = L.findIndex (== 0) list
    index i = list !! (circMod (initial + i) $ length list)

run :: IO ()
run = do
  input <- map readInt <$> lines <$> readFile "inputs/dec20.txt"
  print $ calcValue $ runMoves 1 input
  print $ calcValue $ runMoves 10 $ decrypt input
