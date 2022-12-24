{-# LANGUAGE OverloadedStrings #-}

module Dec18
  ( run
  ) where

import Control.Applicative ((*>), (<*), liftA3)
import Control.Monad.State
import qualified Data.Attoparsec.Text as AP
import qualified Data.List as L
import qualified Data.Map as M
import Data.Map ((!))
import qualified Data.Text as T

data Cube =
  Cube
    { x :: Int
    , y :: Int
    , z :: Int
    }
  deriving (Show, Eq, Ord)

data SpaceType
  = Lava
  | Air
  | Steam
  | Indetermined
  deriving (Eq, Show, Ord)

type Map = M.Map Cube SpaceType

cubeParser :: AP.Parser Cube
cubeParser =
  liftA3
    Cube
    (AP.decimal <* AP.char ',')
    (AP.decimal <* AP.char ',')
    (AP.decimal <* AP.endOfLine)

parseAll :: String -> [Cube]
parseAll text =
  case AP.parseOnly ((AP.many1 cubeParser) <* AP.endOfInput) (T.pack text) of
    Left a -> error a
    Right b -> b

areCovering :: Cube -> Cube -> Bool
areCovering (Cube x1 y1 z1) (Cube x2 y2 z2)
  | x1 == x2 && y1 == y2 && (abs $ z1 - z2) == 1 = True
  | x1 == x2 && z1 == z2 && (abs $ y1 - y2) == 1 = True
  | z1 == z2 && y1 == y2 && (abs $ x1 - x2) == 1 = True
  | otherwise = False

countCoverings :: [Cube] -> Int
countCoverings cubes =
  length $
  filter (uncurry areCovering) [(c1, c2) | c1 <- cubes, c2 <- cubes, c1 /= c2]

minimumProjected :: Ord b => (a -> b) -> [a] -> b
minimumProjected f list = minimum $ map f list

maximumProjected :: Ord b => (a -> b) -> [a] -> b
maximumProjected f list = maximum $ map f list

limits :: [Cube] -> (Cube, Cube)
limits cubes =
  ( Cube
      (minimumProjected x cubes)
      (minimumProjected y cubes)
      (minimumProjected z cubes)
  , Cube
      (maximumProjected x cubes)
      (maximumProjected y cubes)
      (maximumProjected z cubes))

generateMap :: [Cube] -> Map
generateMap cubes = M.fromList [(c, Lava) | c <- cubes]

outOfBounds :: (Cube, Cube) -> Cube -> Bool
outOfBounds (Cube xl yl zl, Cube xu yu zu) (Cube x y z)
  | x < xl || y < yl || z < zl = True
  | x > xu || y > yu || z > zu = True
  | otherwise = False

neighbors :: Cube -> [Cube]
neighbors (Cube x y z) =
  map
    (\(a, b, c) -> Cube a b c)
    [ (x + 1, y, z)
    , (x - 1, y, z)
    , (x, y + 1, z)
    , (x, y - 1, z)
    , (x, y, z + 1)
    , (x, y, z - 1)
    ]

exploreNeighbor :: (Cube, Cube) -> Cube -> State Map SpaceType
exploreNeighbor limits c = do
  m <- get
  let typ = M.lookup c m
  case typ of
    Nothing -> explore limits c
    Just foo -> return $ foo

computeSpace :: [SpaceType] -> SpaceType
computeSpace neighborTypes
  | elem Air neighborTypes && elem Steam neighborTypes = error "Steam & Air"
  | elem Air neighborTypes = Air
  | elem Steam neighborTypes = Steam
  | elem Indetermined neighborTypes = Indetermined
  | otherwise = Air

explore :: (Cube, Cube) -> Cube -> State Map SpaceType
explore limits c = do
  mappy <- get
  let current = M.lookup c mappy
  if elem current [Just Air, Just Steam, Just Lava]
    then return $ mappy ! c
    else do
      if outOfBounds limits c
        then do
          modify $ M.insert c Steam
          return Steam
        else do
          modify $ M.insert c Indetermined
          results <- mapM (exploreNeighbor limits) $ neighbors c
          modify $ M.insert c (computeSpace results)
          return $ computeSpace results

doExplore :: (Cube, Cube) -> State Map ()
doExplore limits = do
  m <- get
  let interesting = concat $ M.mapWithKey (interestingFilter m) m
  if null interesting
    then return ()
    else do
      mapM_ (explore limits) interesting
  where
    interestingFilter :: Map -> Cube -> SpaceType -> [Cube]
    interestingFilter m c t 
        | outOfBounds limits c = []
        | otherwise = filter (\n -> elem (M.lookup n m) [Nothing, Just Indetermined]) $
      neighbors c

exploration :: [Cube] -> Map
exploration cubes = do
  let bounds = limits cubes
  let foo = execState (doExplore bounds) $ generateMap cubes
  execState (doExplore bounds) foo

countFacesToSteam :: Map -> Int
countFacesToSteam m = do
  let steamyNeighbors = M.elems $ M.mapWithKey steamyMap m
  foldl1 (+) $ map length steamyNeighbors
  where
    steamyMap :: Cube -> SpaceType -> [Cube]
    steamyMap c Lava = filter (\n -> elem (M.lookup n m) [Just Steam]) $ neighbors c
    steamyMap _ _ = []

run :: IO ()
run = do
  input <- parseAll <$> readFile "inputs/dec18.txt"
  let mappy = exploration $ input
  print $ (6 * length input) - (countCoverings input)
  print $ countFacesToSteam mappy
