{-# LANGUAGE OverloadedStrings #-}

module Dec19
  ( run
  ) where

import Control.Applicative ((*>), (<*), (<|>))
import qualified Data.Attoparsec.Text as AP
import qualified Data.List as L
import qualified Data.Map as M
import Data.Map ((!))
import qualified Data.Set as S
import qualified Data.Text as T
import Debug.Trace

data Resource
  = Ore
  | Clay
  | Obsidian
  | Geode
  deriving (Show, Ord, Eq)

resources = [Ore, Clay, Obsidian, Geode]

data ResourceMap a =
  ResourceMap
    { rmOre :: a
    , rmClay :: a
    , rmObsidian :: a
    , rmGeode :: a
    }
  deriving (Ord, Eq, Show)

getter = [rmOre, rmClay, rmObsidian, rmGeode]

type RobotCost = ResourceMap Int

nullMap = ResourceMap 0 0 0 0

data Blueprint =
  Blueprint
    { bNumber :: Int
    , bMap :: ResourceMap RobotCost
    }
  deriving (Show)

data CacheKey =
  CacheKey
    { ckResources :: RobotCost
    , ckRobots :: RobotCost
    }
  deriving (Show, Ord, Eq)

initialCacheKey =
  CacheKey {ckResources = nullMap, ckRobots = ResourceMap 1 0 0 0}

type Cache = [CacheKey]

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

add :: RobotCost -> RobotCost -> RobotCost
add (ResourceMap a1 b1 c1 d1) (ResourceMap a2 b2 c2 d2) =
  ResourceMap (a1 + a2) (b1 + b2) (c1 + c2) (d1 + d2)

remove :: RobotCost -> RobotCost -> RobotCost
remove (ResourceMap a1 b1 c1 d1) (ResourceMap a2 b2 c2 d2) =
  ResourceMap (a1 - a2) (b1 - b2) (c1 - c2) (d1 - d2)

produce :: CacheKey -> CacheKey
produce ck = ck {ckResources = add (ckResources ck) (ckRobots ck)}

buy :: Blueprint -> Resource -> CacheKey -> CacheKey
buy bp typ ck =
  ck
    { ckResources = remove (ckResources ck) (typeToGetter typ $ bMap bp)
    , ckRobots = add (ckRobots ck) (typToMap typ 1 0)
    }

possiblePurchases :: Blueprint -> [Resource] -> CacheKey -> [CacheKey]
possiblePurchases bp [] ck = []
possiblePurchases bp (r:esource) ck =
  (possiblePurchases bp esource ck) ++
  if minorizes (typeToGetter r $ bMap bp) (ckResources ck)
    then do
      let newCk = (buy bp r ck)
      newCk : (possiblePurchases bp (esource) newCk)
    else []

optimize :: [CacheKey] -> [CacheKey]
optimize cks =
  [ ck
  | ck@(CacheKey {ckResources = res, ckRobots = rob}) <- cks
  , not $
      any
        (\c ->
           c /= ck &&
           minorizes (res) (ckResources c) && minorizes (rob) (ckRobots c))
        cks
  ]

runRound :: Blueprint -> Cache -> Cache
runRound bp cache =
  traceShow (("RunRound", length cache)) $
  optimize $
  L.nub $
  map produce $
  concat $ map (\c -> c : (possiblePurchases bp resources c)) $ cache

minorizes :: RobotCost -> RobotCost -> Bool
minorizes (ResourceMap a1 b1 c1 d1) (ResourceMap a2 b2 c2 d2) =
  a1 <= a2 && b1 <= b2 && c1 <= c2 && d1 <= d2

isCheaperBlueprint :: Blueprint -> Blueprint -> Bool
isCheaperBlueprint bp1 bp2 =
  all (\r -> minorizes (r $ bMap bp1) (r $ bMap bp2)) $ getter

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

union :: (a -> a -> a) -> ResourceMap a -> ResourceMap a -> ResourceMap a
union f a b = ResourceMap ore clay obsidian geode
  where
    [ore, clay, obsidian, geode] = [f (r a) (r b) | r <- getter]

better :: RobotCost -> RobotCost -> RobotCost
better a b =
  if a == nullMap
    then b
    else a

parseBlueprint :: AP.Parser Blueprint
parseBlueprint = do
  number <- "Blueprint " *> AP.decimal <* ": "
  robots <- AP.sepBy parseRobot ". " <* ".\n"
  return $ Blueprint number $ foldl1 (union (better)) robots

parseAll :: String -> [Blueprint]
parseAll text =
  case AP.parseOnly (AP.many1 parseBlueprint <* AP.endOfInput) (T.pack text) of
    Left a -> error a
    Right b -> b

minimize :: [Blueprint] -> [Blueprint]
minimize bps =
  [ bp
  | bp <- bps
  , not $ any (\b -> bNumber b /= bNumber bp && isCheaperBlueprint b bp) bps
  ]

bestResult :: Cache -> Int
bestResult cache = maximum $ map (\ck -> rmGeode $ ckResources ck) cache

evalBlueprint :: Blueprint -> Int
evalBlueprint bp = do
  let rounds = take 23 $ iterate (runRound bp) $ [initialCacheKey]
  let lastCache = head $ reverse $ rounds
  bestResult lastCache

run :: IO ()
run = do
  input <- parseAll <$> readFile "inputs/dec19.txt"
  print $ length input
  print $ evalBlueprint $ head input
