module Dec23
  ( run
  ) where

import Control.Monad.State
import Data.Function (on)
import qualified Data.List as L
import qualified Data.Map as M
import Data.Map ((!))
import qualified Data.Set as S
import Debug.Trace

data Position =
  Position
    { x :: Int
    , y :: Int
    }
  deriving (Show, Ord, Eq)

data GameState =
  GameState
    { gsElves :: S.Set Position
    , gsPropDirections :: [Position]
    }

initialPropDirections =
  [Position 0 (-1), Position 0 (1), Position (-1) 0, Position 1 0]

allAdjacent = L.nub $ foldl (++) [] $ map wiggle initialPropDirections

readMapLine :: Int -> String -> S.Set Position
readMapLine y line =
  S.fromList
    [Position {x = x, y = y} | (x, field) <- zip [1 ..] line, field == '#']

readMap :: [String] -> S.Set Position
readMap lines = S.unions $ map (uncurry readMapLine) $ zip [1 ..] lines

wiggle :: Position -> [Position]
wiggle (Position 0 y) = [Position dx y | dx <- [0, -1, 1]]
wiggle (Position x 0) = [Position x dy | dy <- [0, -1, 1]]

add :: Position -> Position -> Position
add (Position x1 y1) (Position x2 y2) = Position (x1 + x2) (y1 + y2)

isProposition :: S.Set Position -> Position -> Position -> Bool
isProposition elves direction elve =
  not $ any (\d -> S.member (add d elve) elves) $ wiggle direction

isActive :: S.Set Position -> Position -> Bool
isActive elves elve = any (\d -> S.member (add d elve) elves) allAdjacent

idListMap :: Ord a => a -> M.Map a [a]
idListMap key = M.singleton key [key]

collectPropositions ::
     S.Set Position -> [Position] -> [Position] -> M.Map Position [Position]
collectPropositions _ [] remainingElves =
  M.unions $ map idListMap remainingElves
collectPropositions allElves (d:irections) remainingElves = do
  let (proposed, single) = L.partition (isProposition allElves d) remainingElves
  let mappy = M.fromList $ map (\e -> (add d e, [e])) proposed
  M.unionWith (++) mappy $ collectPropositions allElves irections single

executePropositions :: M.Map Position [Position] -> S.Set Position
executePropositions proposed =
  S.unions $
  M.elems $
  M.mapWithKey
    (\k ->
       \es ->
         if length es == 1
           then S.singleton k
           else S.fromList es)
    proposed

anyMovingPropositions :: M.Map Position [Position] -> Bool
anyMovingPropositions props =
  any (\(k, v) -> length v == 1 && head v /= k) $ M.toList props

wrapDirections :: [Position] -> [Position]
wrapDirections (d:irections) = irections ++ [d]

runRound :: State GameState Bool
runRound = do
  GameState {gsElves = elves, gsPropDirections = directions} <- get
  let (active, inactive) = L.partition (isActive elves) $ S.toList elves
  let propositions = collectPropositions elves directions $ active
  let afterRound =
        executePropositions $
        M.union propositions $ M.unions $ map idListMap inactive
  put
    GameState
      {gsElves = afterRound, gsPropDirections = wrapDirections directions}
  return $ anyMovingPropositions propositions

runNRounds :: Int -> S.Set Position -> S.Set Position
runNRounds rounds elves = do
  let gs = GameState {gsElves = elves, gsPropDirections = initialPropDirections}
  gsElves $ execState (sequence $ replicate rounds runRound) gs

runUntilUnmoving :: State GameState Int
runUntilUnmoving = do
  hadMoving <- runRound
  if hadMoving
    then (+ 1) <$> runUntilUnmoving
    else return 1

findEnd :: S.Set Position -> Int
findEnd elves = do
  let gs = GameState {gsElves = elves, gsPropDirections = initialPropDirections}
  evalState runUntilUnmoving gs

minMaxPos :: Position -> (Position, Position) -> (Position, Position)
minMaxPos (Position x y) (Position minX minY, Position maxX maxY) =
  (Position (min minX x) (min minY y), Position (max maxX x) (max maxY y))

calcSize :: S.Set Position -> Int
calcSize elves = do
  let initial = (\e -> (e, e)) $ S.elemAt 0 elves
  let (minP, maxP) = S.foldr minMaxPos initial elves
  let spanX = x maxP - x minP + 1
  let spanY = y maxP - y minP + 1
  spanX * spanY - S.size elves

showLine :: S.Set Position -> Int -> Int -> Int -> String
showLine elves from to row =
  [ if S.member (Position x row) elves
    then '#'
    else '.'
  | x <- [from .. to]
  ]

run :: IO ()
run = do
  input <- readMap <$> lines <$> readFile "inputs/dec23.txt"
  print $ calcSize $ runNRounds 10 $ input
  print $ findEnd input
