{-# LANGUAGE OverloadedStrings #-}

module Dec14
  ( run
  ) where

import Control.Applicative ((*>), (<*), liftA2)
import qualified Data.Attoparsec.Combinator as AP
import qualified Data.Attoparsec.Text as AP
import Data.Either (fromRight)
import qualified Data.Map as M
import qualified Data.Text as T

data Field
  = Rock
  | Sand
  deriving (Show, Eq)

data Position =
  Position
    { x :: Int
    , y :: Int
    }
  deriving (Show, Ord, Eq)

data FallResult
  = Endless
  | Rested Position

data MapData =
  MapData
    { mdMap :: M.Map Position Field
    , mdMaxY :: Int
    }
  deriving (Show)

startingPoint = Position {x = 500, y = 0}

positionParser :: AP.Parser Position
positionParser = liftA2 Position AP.decimal $ AP.char ',' *> AP.decimal

structureParser :: AP.Parser [Position]
structureParser = AP.sepBy positionParser " -> " <* AP.endOfLine

parseFile :: AP.Parser a -> AP.Parser [a]
parseFile parser = (AP.many' parser <* AP.endOfInput)

readStructures :: T.Text -> [[Position]]
readStructures text =
  case AP.parseOnly (parseFile structureParser) text of
    Left a -> error a
    Right b -> b

range :: Int -> Int -> [Int]
range from to = [(min from to) .. (max from to)]

rangeBetween :: Position -> Position -> [Position]
rangeBetween (Position x1 y1) (Position x2 y2)
  | x1 == x2 = [Position x1 y | y <- range y1 y2]
  | y1 == y1 = [Position x y1 | x <- range x1 x2]
  | otherwise = error "Diagonal lines to supported"

structureToMap :: [Position] -> M.Map Position Field
structureToMap (s:t:ruct) =
  M.union (M.fromList $ zip (rangeBetween s t) (repeat Rock)) $
  structureToMap (t : ruct)
structureToMap _ = M.empty

mapFromStructures :: [[Position]] -> M.Map Position Field
mapFromStructures = M.unions . map structureToMap

nextPositions :: Position -> [Position]
nextPositions (Position x y) = [Position (x + dx) (y + 1) | dx <- [0, -1, 1]]

nextField :: Position -> M.Map Position Field -> Maybe Position
nextField p mappy
  | null next = Nothing
  | otherwise = Just $ head next
  where
    next = filter (\p -> not $ M.member p mappy) $ nextPositions p

fall :: MapData -> Position -> FallResult
fall md@(MapData mappy maxY) p
  | y p > maxY = Endless
  | otherwise = maybe (Rested p) (fall md) $ nextField p mappy

runFall :: MapData -> MapData
runFall md@(MapData mappy maxY) =
  case result of
    Endless -> md
    Rested p -> runFall $ md {mdMap = (M.insert p Sand mappy)}
  where
    result = fall md startingPoint

fall2 :: MapData -> Position -> Position
fall2 md@(MapData mappy maxY) p
  | y p == maxY + 1 = p
  | otherwise = maybe p (fall2 md) $ nextField p mappy

runFall2 :: MapData -> MapData
runFall2 md@(MapData mappy maxY)
  | p == startingPoint = result
  | otherwise = runFall2 result
  where
    p = fall2 md startingPoint
    result = md {mdMap = M.insert p Sand mappy}

generateMapData :: M.Map Position Field -> MapData
generateMapData mappy = MapData mappy $ maximum $ map y $ M.keys mappy

countSand :: MapData -> Int
countSand = length . filter (== Sand) . M.elems . mdMap

run :: IO ()
run = do
  input <-
    generateMapData <$> mapFromStructures <$> readStructures <$> T.pack <$>
    readFile "inputs/dec14.txt"
  print $ countSand $ runFall input
  print $ countSand $ runFall2 input
