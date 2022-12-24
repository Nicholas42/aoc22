{-# LANGUAGE OverloadedStrings #-}

module Dec19
  ( run
  ) where

import Control.Applicative ((*>), (<*), (<|>))
import Control.Monad.State
import Control.Parallel.Strategies
import qualified Data.Attoparsec.Text as AP
import Data.Function (on)
import qualified Data.List as L
import qualified Data.Map as M
import Data.Map ((!))
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import Debug.Trace

data Resource
  = Ore
  | Clay
  | Obsidian
  | Geode
  deriving (Show, Ord, Eq)

types = [Ore, Clay, Obsidian, Geode]

data ResourceMap a =
  ResourceMap
    { rmOre :: a
    , rmClay :: a
    , rmObsidian :: a
    , rmGeode :: a
    }
  deriving (Ord, Eq, Show)

getters = [rmOre, rmClay, rmObsidian, rmGeode]

type RobotCost = ResourceMap Int

nullMap = ResourceMap 0 0 0 0

data Blueprint =
  Blueprint
    { bNumber :: Int
    , bMap :: ResourceMap RobotCost
    }
  deriving (Show)

typToMap :: Resource -> a -> a -> ResourceMap a
typToMap typ special normal =
  case typ of
    Ore -> ResourceMap special normal normal normal
    Clay -> ResourceMap normal special normal normal
    Obsidian -> ResourceMap normal normal special normal
    Geode -> ResourceMap normal normal normal special

typeToGetter :: Resource -> (ResourceMap a -> a)
typeToGetter typ =
  case typ of
    Ore -> rmOre
    Clay -> rmClay
    Obsidian -> rmObsidian
    Geode -> rmGeode

notNullMap :: RobotCost -> RobotCost -> RobotCost
notNullMap (ResourceMap 0 0 0 0) a = a
notNullMap a (ResourceMap 0 0 0 0) = a

union :: (a -> a -> a) -> ResourceMap a -> ResourceMap a -> ResourceMap a
union f a b = ResourceMap ore clay obsidian geode
  where
    [ore, clay, obsidian, geode] = [f (r a) (r b) | r <- getters]

add :: RobotCost -> RobotCost -> RobotCost
add (ResourceMap a1 b1 c1 d1) (ResourceMap a2 b2 c2 d2) =
  ResourceMap (a1 + a2) (b1 + b2) (c1 + c2) (d1 + d2)

remove :: RobotCost -> RobotCost -> RobotCost
remove (ResourceMap a1 b1 c1 d1) (ResourceMap a2 b2 c2 d2) =
  ResourceMap (a1 - a2) (b1 - b2) (c1 - c2) (d1 - d2)

scalarMult :: Int -> RobotCost -> RobotCost
scalarMult s (ResourceMap a b c d) = ResourceMap (s * a) (s * b) (s * c) (s * d)

parseResource :: AP.Parser Resource
parseResource = do
  word <- "ore" <|> "clay" <|> "obsidian" <|> "geode"
  return $
    case word of
      "ore" -> Ore
      "clay" -> Clay
      "obsidian" -> Obsidian
      "geode" -> Geode
      _ -> error "AP fail"

parseCost :: AP.Parser RobotCost
parseCost = do
  number <- AP.decimal <* AP.skipSpace
  typ <- parseResource
  return $ typToMap typ number 0

parseRobot :: AP.Parser (ResourceMap RobotCost)
parseRobot = do
  typ <- "Each " *> parseResource <* " robot costs "
  resources <- AP.sepBy parseCost " and "
  return $ typToMap typ (foldl1 add resources) nullMap

parseBlueprint :: AP.Parser Blueprint
parseBlueprint = do
  number <- "Blueprint " *> AP.decimal <* ": "
  robots <- AP.sepBy parseRobot ". " <* ".\n"
  return $ Blueprint number $ foldl1 (union notNullMap) robots

parseAll :: String -> [Blueprint]
parseAll text =
  case AP.parseOnly (AP.many1 parseBlueprint <* AP.endOfInput) (T.pack text) of
    Left a -> error a
    Right b -> b

type Cache = M.Map RobotCost [RobotCost]

majorizes :: Ord a => ResourceMap a -> ResourceMap a -> Bool
majorizes (ResourceMap o1 c1 b1 g1) (ResourceMap o2 c2 b2 g2) =
  o1 >= o2 && c1 >= c2 && b1 >= b2 && g1 >= g2

addToCache :: RobotCost -> RobotCost -> Cache -> Cache
addToCache robots resources cache = do
  let existingResources = M.findWithDefault [] robots cache
  if not $ any (\rc -> rc `majorizes` resources) existingResources
    then M.insert robots (resources : existingResources) cache
    else cache

zeroSafeDiv :: Int -> Int -> Int
zeroSafeDiv a 0 = 100000000
zeroSafeDiv a b = a `div` b

maxBuyable :: RobotCost -> RobotCost -> Int
maxBuyable prices resources =
  minimum $ map (\f -> (zeroSafeDiv `on` f) resources prices) getters

buyMultiple ::
     ResourceMap RobotCost
  -> RobotCost
  -> RobotCost
  -> Maybe (RobotCost, RobotCost)
buyMultiple prices resources request = do
  let totalCosts =
        foldl1
          (add)
          [ (typeToGetter t request) `scalarMult` (typeToGetter t prices)
          | t <- types
          ]
  if resources `majorizes` totalCosts
    then Just $ (request, resources `remove` totalCosts)
    else Nothing

candidates ::
     ResourceMap RobotCost -> RobotCost -> RobotCost -> [(RobotCost, RobotCost)]
candidates prices robots resources = do
  let bought =
        map (\(rob, res) -> (robots `add` rob, robots `add` res)) $
        mapMaybe (\t -> buyMultiple prices resources (typToMap t 1 0)) types
  if length bought < length types
    then (robots, robots `add` resources) : bought
    else bought

candidates1 ::
     ResourceMap RobotCost -> RobotCost -> RobotCost -> [(RobotCost, RobotCost)]
candidates1 prices robots resources = do
  let bounds@[ore, clay, obs, geode] =
        [maxBuyable (f prices) resources | f <- getters]
  let canWait = any (== 0) bounds
  let bought =
        mapMaybe
          (buyMultiple prices resources)
          [ (ResourceMap o c b g)
          | o <- [0 .. ore]
          , c <- [0 .. clay]
          , b <- [0 .. obs]
          , g <- [0 .. geode]
          , canWait || (not $ all (== 0) [o, c, b, g])
          ]
  map (\(rob, res) -> (robots `add` rob, res)) bought

runRound :: ResourceMap RobotCost -> Cache -> Cache
runRound prices cache = do
  let allCandidates =
        concat $
        concat $
        map (\(rob, ress) -> map (candidates prices rob) ress) $ M.toList cache
  L.foldl' (\c -> \(rob, res) -> addToCache rob res c) M.empty allCandidates

initialCache :: Cache
initialCache = M.singleton (typToMap Ore 1 0) [nullMap]

calcCacheValue :: Cache -> Int
calcCacheValue cache = maximum $ map rmGeode $ concat $ M.elems cache

evalBlueprint :: Blueprint -> Int
evalBlueprint (Blueprint {bNumber = number, bMap = prices}) = do
  let finalCache = (iterate (runRound prices) initialCache) !! 24
  number * (calcCacheValue finalCache)

run :: IO ()
run = do
  input <- parseAll <$> readFile "inputs/dec19.txt"
  print $ length input
  print $ sum $ parMap rpar evalBlueprint input
