module Dec12
  ( run
  ) where

import Control.Monad.State
import qualified Data.Map as M
import Data.Map ((!))
import qualified Data.Sequence as S
import Data.Sequence ((<|), (><), (|>))
import Debug.Trace
import Data.Maybe (mapMaybe)

data Position =
  Position
    { x :: Int
    , y :: Int
    }
  deriving (Show, Eq, Ord)

type Map = M.Map Position Char

type VisitorMap = M.Map Position Bool

type DistanceMap = M.Map Position Int

data Edge =
  Edge
    { from :: Maybe Position
    , to :: Position
    }
  deriving (Show)

data MapData =
  MapData
    { mapp :: Map
    , distances :: DistanceMap
    , visitors :: VisitorMap
    , frontier :: S.Seq Edge
    , end :: Position
    }

highest :: Char
highest = succ $ succ 'z'

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]

readMapLine :: String -> Int -> Map
readMapLine line y = M.fromList [(Position x y, c) | (x, c) <- enumerate line]

readMap :: [String] -> Map
readMap lines = M.unions [readMapLine line y | (y, line) <- enumerate lines]

visitorMap :: Map -> VisitorMap
visitorMap = M.map (const False)

findStartAndEnd :: Map -> MapData
findStartAndEnd mappy = do
  let [(startPos, _)] = M.toList $ M.filter (== 'S') mappy
  let [(endPos, _)] = M.toList $ M.filter (== 'E') mappy
  let newMap = M.insert startPos 'a' $ M.insert endPos 'z' mappy
  let visitors = visitorMap mappy
  let distances = M.empty
  let front = S.singleton (Edge Nothing startPos)
  MapData newMap distances visitors front endPos

startAt :: MapData -> Position -> MapData
startAt dat pos = dat {frontier = S.singleton (Edge Nothing pos)}

neighborPos :: Position -> [Position]
neighborPos (Position x y) =
  map (uncurry Position) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

findNeighbors :: Map -> Position -> [Position]
findNeighbors mapp pos =
  filter (\k -> (M.findWithDefault highest k mapp) <= limit) $ neighborPos pos
  where
    limit = succ $ mapp ! pos

distUpdate :: Int -> Maybe Int -> Maybe Int
distUpdate x Nothing = Just x
distUpdate x (Just y) = Just $ min x y

visit :: VisitorMap -> Position -> VisitorMap
visit visitors pos = M.insert pos True visitors

updateDistances :: DistanceMap -> Edge -> DistanceMap
updateDistances distances (Edge Nothing to) = M.insert to 0 distances
updateDistances distances (Edge (Just from) to) =
  M.alter (distUpdate $ succ $ distances ! from) to distances

bfs :: State MapData ()
bfs = do
  dat <- get
  if S.null $ frontier dat
    then return ()
    else do
      let front = S.drop 1 $ frontier dat
      let cur = S.index (frontier dat) 0
      put dat {frontier = front}
      if (visitors dat) ! (to cur)
        then return ()
        else do
          let visited = visit (visitors dat) (to cur)
          let distanced = updateDistances (distances dat) cur
          let toVisit =
                S.fromList
                  [ Edge (Just $ to cur) n
                  | n <- findNeighbors (mapp dat) (to cur)
                  ]
          put
            dat
              { visitors = visited
              , distances = distanced
              , frontier = front >< toVisit
              }

runBfs :: State MapData (Maybe Int)
runBfs = do
  dat <- get
  let atEnd = \m -> m ! (end dat)
  if atEnd (visitors dat)
    then return $ Just $ atEnd $ distances dat
    else do 
        if S.null (frontier dat) then return Nothing else bfs >> runBfs

multiSourceBfs :: MapData -> Int
multiSourceBfs dat = do
  let sources = M.keys $ M.filter (== 'a') $ mapp dat
  minimum $ mapMaybe (\s -> evalState runBfs $ startAt dat s) sources

run :: IO ()
run = do
  input <- readMap <$> lines <$> readFile "inputs/dec12.txt"
  let dat = findStartAndEnd input
  print $ evalState runBfs dat
  print $ multiSourceBfs dat
