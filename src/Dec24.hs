module Dec24
  ( run
  ) where

import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S
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
  deriving (Show, Eq)

data Field
  = Free
  | Wall
  | Blizzard Direction
  deriving (Show, Eq)

data Boundary =
  Boundary
    { bMin :: Position
    , bMax :: Position
    }
  deriving (Show, Eq)

data GameState =
  GameState
    { gsBlizzards :: M.Map Position [Direction]
    , gsFrontier :: S.Set Position
    }

readField :: Char -> Field
readField c =
  case c of
    '.' -> Free
    '#' -> Wall
    '^' -> Blizzard North
    '>' -> Blizzard East
    'v' -> Blizzard South
    '<' -> Blizzard West

readLine :: (Int, String) -> M.Map Position Field
readLine (y, line) =
  M.fromList [(Position x y, readField c) | (x, c) <- zip [0 ..] line]

readMap :: [String] -> M.Map Position Field
readMap lines = M.unions $ map readLine $ zip [0 ..] lines

extractDirection :: Field -> Maybe Direction
extractDirection (Blizzard d) = Just d
extractDirection _ = Nothing

extractBlizzards :: M.Map Position Field -> M.Map Position [Direction]
extractBlizzards mappy = M.map (\d -> [d]) $ M.mapMaybe extractDirection mappy

extractBoundary :: M.Map Position Field -> Boundary
extractBoundary mappy =
  Boundary {bMin = fst $ M.findMin mappy, bMax = fst $ M.findMax mappy}

isOutOfBounds :: Boundary -> Position -> Bool
isOutOfBounds (Boundary {bMin = Position xmin ymin, bMax = Position xmax ymax}) (Position x y)
  | x <= xmin || y < ymin || x >= xmax || y > ymax = True
  | y == ymin = x /= xmin + 1
  | y == ymax = x /= xmax - 1
  | otherwise = False

moveOutOfBoundary :: Boundary -> Position -> Position
moveOutOfBoundary (Boundary { bMin = Position xmin ymin
                            , bMax = Position xmax ymax
                            }) (Position x y)
  | xmin == x = Position (xmax - 1) y
  | ymin == y = Position x (ymax - 1)
  | xmax == x = Position (xmin + 1) y
  | ymax == y = Position x (ymin + 1)
  | otherwise = Position x y

moveBlizzard :: Boundary -> Position -> Direction -> Position
moveBlizzard bounds pos dir =
  moveOutOfBoundary bounds $
  Position {x = x pos + fst delta, y = y pos + snd delta}
  where
    delta =
      case dir of
        North -> (0, -1)
        East -> (1, 0)
        South -> (0, 1)
        West -> (-1, 0)

nextMoves :: Boundary -> Position -> S.Set Position
nextMoves bounds (Position x y) =
  S.fromList $
  filter
    (not . isOutOfBounds bounds)
    [ Position (x + dx) (y + dy)
    | (dx, dy) <- [(0, 0), (1, 0), (-1, 0), (0, 1), (0, -1)]
    ]

moveFrontier ::
     Boundary -> M.Map Position [Direction] -> S.Set Position -> S.Set Position
moveFrontier bounds blizzards frontier = do
  let nextFrontier = S.unions $ S.map (nextMoves bounds) frontier
  S.difference nextFrontier (M.keysSet blizzards)

moveBlizzards ::
     Boundary -> M.Map Position [Direction] -> M.Map Position [Direction]
moveBlizzards bounds blizzards =
  M.fromListWith (++) $
  concat $
  M.elems $
  M.mapWithKey
    (\k -> \vs -> [(moveBlizzard bounds k v, [v]) | v <- vs])
    blizzards

runRound :: Boundary -> State GameState ()
runRound bounds = do
  modify $ \g -> g {gsBlizzards = moveBlizzards bounds $ gsBlizzards g}
  modify $ \g ->
    g {gsFrontier = moveFrontier bounds (gsBlizzards g) (gsFrontier g)}

source :: Boundary -> Position
source (Boundary {bMin = Position xmin ymin}) = Position (xmin + 1) ymin

target :: Boundary -> Position
target (Boundary {bMax = Position xmax ymax}) = Position (xmax - 1) ymax

runToTarget :: Boundary -> Position -> State GameState Int
runToTarget bounds t = do
  frontier <- gsFrontier <$> get
  if S.member t frontier
    then modify (\g -> g {gsFrontier = S.singleton t}) >> return 0
    else runRound bounds >> (+ 1) <$> runToTarget bounds t

andBackAgain :: Boundary -> State GameState Int
andBackAgain bounds = do
  trip1 <- runToTarget bounds $ source bounds
  trip2 <- runToTarget bounds $ target bounds
  return $ trip1 + trip2

run :: IO ()
run = do
  mappy <- readMap <$> lines <$> readFile "inputs/dec24.txt"
  let bounds = extractBoundary mappy
  let blizzards = extractBlizzards mappy
  let s = source bounds
  let gs = GameState blizzards $ S.singleton s
  let (result, there) = runState (runToTarget bounds $ target bounds) gs
  print result
  print $ result + evalState (andBackAgain bounds) there
