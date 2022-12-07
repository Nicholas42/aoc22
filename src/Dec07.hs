module Dec07
  ( run
  ) where

import Data.List (isPrefixOf, isSuffixOf, nub, sort, sortBy)
import Data.Map (Map, (!), alter, elems, empty, keys, member, notMember)

data Path =
  Path [String]
  deriving (Show, Ord, Eq)

data File =
  File
    { size :: Int
    , path :: Path
    }
  deriving (Show, Eq)

instance Ord File where
  x <= y = (path x) <= (path y)

parent :: Path -> Path
parent (Path path) = Path $ tail path

appendPath :: Path -> String -> Path
appendPath (Path path) new
  | new == ".." = parent (Path path)
  | new == "/" = Path []
  | otherwise = Path (new : path)

readFileLine :: String -> Path -> File
readFileLine line path = do
  let [size, _] = words line
  File (read size) path

handleCd :: Path -> String -> Path
handleCd path line = do
  let [_, _, name] = words line
  appendPath path name

handleLs :: [String] -> Path -> [File] -> ([String], [File])
handleLs [] _ files = ([], files)
handleLs (l:ines) path files
  | isPrefixOf "dir" l = handleLs ines path files
  | isPrefixOf "$" l = handleCmd (l : ines) path files
  | otherwise = handleLs ines path ((readFileLine l path) : files)

handleCmd :: [String] -> Path -> [File] -> ([String], [File])
handleCmd [] _ files = ([], files)
handleCmd (l:ines) path files
  | isPrefixOf "$ cd" l = handleCmd ines (handleCd path l) files
  | isPrefixOf "$ ls" l = handleLs ines path files
  | otherwise = error "wrong mode"

addOrConst :: Int -> Maybe Int -> Maybe Int
addOrConst toAdd (Just there) = Just $ there + toAdd
addOrConst toAdd Nothing = Just toAdd

sizeMap :: [File] -> Map Path Int
sizeMap [] = empty
sizeMap (f:iles) = do
  let map = sizeMap iles
  alter (addOrConst $ size f) (path f) map

addSizeToParent :: Int -> Map Path Int -> Path -> Map Path Int
addSizeToParent amount mappy (Path []) = mappy
addSizeToParent amount mappy p =
  alter (addOrConst amount) (parent p) $ addSizeToParent amount mappy $ parent p

consolidateMap :: Map Path Int -> Map Path Int
consolidateMap mapOfSizes = do
  let paths =
        sortBy (\(Path p) (Path q) -> (length p) `compare` (length q)) $
        keys mapOfSizes
  foldl
    (\mappy -> \key -> addSizeToParent (mappy ! key) mappy key)
    mapOfSizes
    paths

partOneLimit = 100000

partTwoMaxUseable = 70000000 - 30000000

run :: IO ()
run = do
  input <- readFile "inputs/dec07.txt"
  let (_, files) = handleCmd (lines input) (Path []) []
  let mapOfSizes = sizeMap files
  let consolidated = consolidateMap mapOfSizes
  print $ sum $ filter (<= partOneLimit) $ elems consolidated
  let used = consolidated ! (Path [])
  print $ minimum $ filter (>= used - partTwoMaxUseable) $ elems consolidated
