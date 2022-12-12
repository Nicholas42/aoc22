module Dec12
  ( run
  ) where

import Control.Monad.State
import qualified Data.Map as M
import Data.Map ((!))
import Data.Maybe (catMaybes)
import qualified Data.Sequence as Seq
import Data.Sequence (ViewL((:<)), (><))
import qualified Data.Set as Set
import Debug.Trace

data Position =
  Position
    { x :: Int
    , y :: Int
    }
  deriving (Show, Eq, Ord)

data Edge =
  Edge
    { from :: Maybe Position
    , to :: Position
    }
  deriving (Show)

type Map = M.Map Position Char

type VisitorSet = Set.Set Position

type DistanceMap = M.Map Position Int

type Frontier = Seq.Seq Edge

data Searcher =
  Searcher
    { sMap :: Map
    , sDistances :: DistanceMap
    , sVisited :: VisitorSet
    , sFrontier :: Frontier
    }

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]

readMapLine :: String -> Int -> Map
readMapLine line y = M.fromList [(Position x y, c) | (x, c) <- enumerate line]

readMap :: [String] -> Map
readMap lines = M.unions [readMapLine line y | (y, line) <- enumerate lines]

initSearcher :: Map -> (Position, Searcher)
initSearcher mappy = do
  let startPos = head $ M.keys $ M.filter (== 'S') mappy
  let endPos = head $ M.keys $ M.filter (== 'E') mappy
  let newMap = M.insert startPos 'a' $ M.insert endPos 'z' mappy
  let frontier = Seq.singleton $ Edge Nothing endPos
  (startPos, Searcher newMap M.empty Set.empty frontier)

neighborPos :: Position -> [Position]
neighborPos (Position x y) =
  [Position (x + dx) (y + dy) | (dx, dy) <- [(1, 0), (-1, 0), (0, 1), (0, -1)]]

findCanReachMe :: Map -> Position -> [Position]
findCanReachMe mapp pos =
  [p | p <- neighborPos pos, maybe False limitP $ M.lookup p mapp]
  where
    limitP = (>= (pred $ mapp ! pos))

visit :: Edge -> State Searcher ()
visit Edge {to = pos} =
  modify $ \d -> d {sVisited = Set.insert pos $ sVisited d}

updateDistances :: Edge -> State Searcher ()
updateDistances (Edge Nothing to) =
  modify $ \d -> d {sDistances = M.insert to 0 $ sDistances d}
updateDistances (Edge (Just from) to) =
  modify $ \d@Searcher {sDistances = dist} ->
    d {sDistances = M.insertWith min to (succ $ dist ! from) dist}

edgesFrom :: Position -> [Position] -> Frontier
edgesFrom from tos = Seq.fromList $ map (Edge (Just from)) tos

searchFrom :: Edge -> State Searcher ()
searchFrom cur@Edge {to = to} = do
  visit cur
  updateDistances cur
  modify $ \d@Searcher {sFrontier = frontier, sMap = mappy} ->
    d {sFrontier = frontier >< (edgesFrom to $ findCanReachMe mappy to)}

bfs :: State Searcher ()
bfs = do
  Searcher {sFrontier = frontier, sVisited = visited} <- get
  let cur :< front = Seq.viewl frontier
  modify $ \d -> d {sFrontier = front}
  if Set.member (to cur) visited
    then return ()
    else searchFrom cur

runBfs :: Set.Set Position -> State Searcher Int
runBfs toSearch = do
  Searcher {sVisited = visited, sFrontier = frontier, sDistances = dist} <- get
  let found = Set.intersection toSearch visited
  if Set.null found
    then do
      if Seq.null frontier
        then error "Cannot reach any target"
        else bfs >> runBfs toSearch
    else return $ minimum $ Set.map ((!) dist) found

allLows :: Searcher -> Set.Set Position
allLows dat = M.keysSet $ M.filter (== 'a') $ sMap dat

run :: IO ()
run = do
  input <- readMap <$> lines <$> readFile "inputs/dec12.txt"
  let (start, dat) = initSearcher input
  print $ evalState (runBfs $ Set.singleton start) dat
  print $ evalState (runBfs $ allLows dat) dat
