module Dec09
  ( run
  ) where

import Control.Monad.State (State, modify, state, evalState, get)
import Data.List (nub)

data Direction
  = Up
  | Down
  | Lefty
  | Righty
  deriving (Show)

data Step =
  Step Direction Int
  deriving (Show)

data Position =
  Position
    { x :: Int
    , y :: Int
    }
  deriving (Show, Eq)

type Rope = [Position]

biMapPos :: (Int -> Int -> Int) -> Position -> Position -> Position
biMapPos f p q = Position (f (x p) (x q)) (f (y p) (y q))

readDirection :: Char -> Direction
readDirection c =
  case c of
    'U' -> Up
    'D' -> Down
    'L' -> Lefty
    'R' -> Righty

readStep :: String -> Step
readStep line = Step (readDirection $ head line) (read $ tail line)

goDirection :: Direction -> Position -> Position
goDirection d (Position x y) =
  case d of
    Up -> Position x $ y + 1
    Down -> Position x $ y - 1
    Lefty -> Position (x - 1) y
    Righty -> Position (x + 1) y

goNextTo :: Int -> Int -> Int
goNextTo target source
  | target < source = target + 1
  | target > source = target - 1
  | otherwise = target

goClose :: Int -> Int -> Int
goClose target source
  | target < source - 1 = source - 1
  | target > source + 1 = source + 1
  | target == source - 1 || target == source + 1 = target

areClose :: Position -> Position -> Bool
areClose a b = (abs $ x a - x b) + (abs $ y a - y b) <= 2

follow :: Position -> Position -> Position
follow head tail
  | areClose head tail = biMapPos goNextTo head tail
  | otherwise = biMapPos goClose head tail

followRope :: Position -> Rope -> Rope
followRope r (o:pe) = r : (followRope (follow r o) pe)
followRope r _ = [r]

moveRope :: Direction -> Rope -> Rope
moveRope d (r:ope) = followRope (goDirection d r) ope

stepsToDirections :: [Step] -> [Direction]
stepsToDirections [] = []
stepsToDirections ((Step d amount):teps) =
  (replicate amount d) ++ stepsToDirections teps

type SimplerRopeState = State Rope Position

executeStep :: Direction -> SimplerRopeState
executeStep d = do 
    modify $ moveRope d
    last <$> get

startRope :: Int -> Rope
startRope length = replicate length $ Position 0 0

executeSteps :: [Direction] -> Int -> [Position]
executeSteps directions length =
  evalState (mapM executeStep directions) $ startRope length

run :: IO ()
run = do
  input <- lines <$> readFile "inputs/dec09.txt"
  let directions = stepsToDirections $ map readStep input
  print $ length.nub $ executeSteps directions 2
  print $ length.nub $ executeSteps directions 10
