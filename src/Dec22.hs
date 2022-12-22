module Dec22
  ( run
  ) where

import Control.Monad.State
import Data.Char (isDigit)
import qualified Data.List as L
import qualified Data.Map as M
import Data.Map ((!))
import Data.Maybe (fromJust, isJust)
import Debug.Trace

data Position =
  Position
    { x :: Int
    , y :: Int
    }
  deriving (Show, Ord, Eq)

data Direction
  = North
  | East
  | South
  | West
  deriving (Show)

data Instruction
  = Go Int
  | TurnLeft
  | TurnRight
  deriving (Show)

data Field
  = Wall
  | Free
  deriving (Show, Eq)

data GameState =
  GameState
    { gsMap :: M.Map Position Field
    , gsFacing :: Direction
    , gsPosition :: Position
    }

readField :: Char -> Maybe Field
readField '.' = Just Free
readField '#' = Just Wall
readField ' ' = Nothing

readInstruction :: String -> Instruction
readInstruction "L" = TurnLeft
readInstruction "R" = TurnRight
readInstruction x = Go $ read x

readInstructions :: String -> [Instruction]
readInstructions line =
  map readInstruction $ L.groupBy (\a -> \b -> isDigit a == isDigit b) line

readMapLine :: Int -> String -> M.Map Position Field
readMapLine y line =
  M.fromList
    [ (Position {x = x, y = y}, fromJust field)
    | (x, field) <- zip [1 ..] $ map readField line
    , isJust field
    ]

readMap :: [String] -> M.Map Position Field
readMap lines = M.unions $ map (uncurry readMapLine) $ zip [1 ..] lines

findStart :: M.Map Position Field -> Position
findStart mappy = minimum $ filter (\p -> y p == 1) $ M.keys mappy

parseInput :: String -> (GameState, [Instruction])
parseInput text = do
  let (mapInput, instructionInput) = L.break (== "") $ lines text
  let mappy = readMap mapInput
  (,) GameState {gsMap = mappy, gsFacing = East, gsPosition = findStart mappy} $
    readInstructions $ instructionInput !! 1

turnRight :: Direction -> Direction
turnRight dir =
  case dir of
    East -> South
    South -> West
    West -> North
    North -> East

turnLeft dir =
  case dir of
    East -> North
    North -> West
    West -> South
    South -> East

stepTo :: Position -> Direction -> Position
stepTo (Position x y) East = Position (x + 1) y
stepTo (Position x y) South = Position x (y + 1)
stepTo (Position x y) West = Position (x - 1) y
stepTo (Position x y) North = Position x (y - 1)

findWrap :: Position -> Direction -> M.Map Position Field -> Position
findWrap pos dir mappy =
  case dir of
    East -> minimum $ filter (\p -> y p == y pos) $ M.keys mappy
    South -> minimum $ filter (\p -> x p == x pos) $ M.keys mappy
    West -> maximum $ filter (\p -> y p == y pos) $ M.keys mappy
    North -> maximum $ filter (\p -> x p == x pos) $ M.keys mappy

findWrapCube :: Position -> Direction -> (Position, Direction)
findWrapCube pos North
  | x pos <= 50 = (Position {x = 51, y = 50 + x pos}, East)
  | x pos <= 100 = (Position {x = 1, y = 100 + x pos}, East)
  | otherwise = (Position {x = (x pos) - 100, y = 200}, North)
findWrapCube pos South
  | x pos <= 50 = (Position {x = (x pos) + 100, y = 1}, South)
  | x pos <= 100 = (Position {x = 50, y = 100 + (x pos)}, West)
  | otherwise = (Position {x = 100, y = (x pos) - 50}, West)
findWrapCube pos East
  | y pos <= 50 = (Position {x = 100, y = 151 - (y pos)}, West)
  | y pos <= 100 = (Position {x = 50 + (y pos), y = 50}, North)
  | y pos <= 150 = (Position {x = 150, y = 151 - (y pos)}, West)
  | otherwise = (Position {x = (y pos) - 100, y = 150}, North)
findWrapCube pos West
  | y pos <= 50 = (Position {x = 1, y = 151 - (y pos)}, East)
  | y pos <= 100 = (Position {x = (y pos) - 50, y = 101}, South)
  | y pos <= 150 = (Position {x = 51, y = 151 - (y pos)}, East)
  | otherwise = (Position {x = (y pos) - 100, y = 1}, South)

makeStep :: State GameState ()
makeStep = do
  GameState {gsMap = mappy, gsPosition = pos, gsFacing = dir} <- get
  let stepped = stepTo pos dir
  let nextPos =
        if M.member stepped mappy
          then stepped
          else findWrap pos dir mappy
  if mappy ! nextPos == Free
    then modify $ (\gs -> gs {gsPosition = nextPos})
    else return ()

makeCubeStep :: State GameState ()
makeCubeStep = do
  GameState {gsMap = mappy, gsPosition = pos, gsFacing = dir} <- get
  let stepped = stepTo pos dir
  let (nextPos, nextDir) =
        if M.member stepped mappy
          then (stepped, dir)
          else findWrapCube pos dir
  if mappy ! nextPos == Free
    then modify $ (\gs -> gs {gsPosition = nextPos, gsFacing = nextDir})
    else return ()

executeInstruction :: Instruction -> State GameState ()
executeInstruction TurnLeft =
  modify $ \gs -> gs {gsFacing = turnLeft $ gsFacing gs}
executeInstruction TurnRight =
  modify $ \gs -> gs {gsFacing = turnRight $ gsFacing gs}
executeInstruction (Go steps) = do
  sequence_ $ replicate steps makeStep

executeInstructions :: GameState -> [Instruction] -> GameState
executeInstructions gs instructions =
  execState (mapM_ executeInstruction instructions) gs

executeInstructionOnCube :: Instruction -> State GameState ()
executeInstructionOnCube TurnLeft =
  modify $ \gs -> gs {gsFacing = turnLeft $ gsFacing gs}
executeInstructionOnCube TurnRight =
  modify $ \gs -> gs {gsFacing = turnRight $ gsFacing gs}
executeInstructionOnCube (Go steps) = do
  sequence_ $ replicate steps makeCubeStep
  foo <- get
  return ()

executeInstructionsOnCube :: GameState -> [Instruction] -> GameState
executeInstructionsOnCube gs instructions =
  execState (mapM_ executeInstructionOnCube instructions) gs

dirValue :: Direction -> Int
dirValue dir =
  case dir of
    East -> 0
    South -> 1
    West -> 2
    North -> 3

calcValue :: GameState -> Int
calcValue (GameState {gsPosition = pos, gsFacing = dir}) =
  1000 * (y pos) + 4 * (x pos) + dirValue dir

run :: IO ()
run = do
  (gameState, instructions) <- parseInput <$> readFile "inputs/dec22.txt"
  let finalGS = executeInstructions gameState instructions
  print $ calcValue finalGS
  let finalCube = executeInstructionsOnCube gameState instructions
  print $ calcValue finalCube
