{-# LANGUAGE OverloadedStrings #-}

module Dec21
  ( run
  ) where

import Control.Applicative ((*>), (<*), (<|>), liftA)
import Control.Monad.State
import Data.Attoparsec.Combinator ((<?>))
import qualified Data.Attoparsec.Text as AP
import Data.Function (on)
import qualified Data.Map as M
import Data.Map ((!))
import qualified Data.Text as T
import Debug.Trace

data Monkey
  = Leaf
      { name :: String
      , value :: Integer
      }
  | Node
      { name :: String
      , lhs :: String
      , op :: Char
      , rhs :: String
      }
  deriving (Show)

parseNumberMonkey :: AP.Parser Monkey
parseNumberMonkey = do
  num <- AP.decimal
  return $ Leaf {name = "", value = num}

parseOpMonkey :: AP.Parser Monkey
parseOpMonkey = do
  lhs <- word <* " "
  op <- AP.anyChar
  rhs <- " " *> word
  return $ Node {name = "", lhs = lhs, op = op, rhs = rhs}

word :: AP.Parser String
word = AP.many1 AP.letter

parseMonkey :: AP.Parser (String, Monkey)
parseMonkey = do
  name <- word <* ": "
  next <- AP.peekChar
  monkey <- (parseOpMonkey <|> parseNumberMonkey) <* AP.endOfLine
  return $ (name, monkey {name = name})

parseAllMonkeys :: T.Text -> M.Map String Monkey
parseAllMonkeys text =
  case AP.parseOnly ((AP.many1 parseMonkey) <* AP.endOfInput) text of
    Left a -> error a
    Right b -> M.fromList b

getOp :: Char -> (Integer -> Integer -> Integer)
getOp '+' = (+)
getOp '-' = (-)
getOp '*' = (*)
getOp '/' = div

evalMonkey :: M.Map String Monkey -> String -> Integer
evalMonkey mappy name = do
  let monkey = mappy ! name
  case monkey of
    Leaf {value = a} -> a
    Node {lhs = lhs, op = op, rhs = rhs} ->
      ((getOp op) `on` (evalMonkey mappy)) lhs rhs

tryNumber :: M.Map String Monkey -> Integer -> Bool
tryNumber mappy human = do
  let newMap = M.insert "humn" (Leaf {name = "humn", value = human}) mappy
  0 == evalMonkey newMap "root"

dependsOn :: M.Map String Monkey -> String -> String -> Bool
dependsOn mappy target source = do
  if target == source
    then True
    else case mappy ! source of
           Leaf {} -> False
           Node {lhs = lhs, rhs = rhs} ->
             any (dependsOn mappy target) [lhs, rhs]

simplifyOp :: Monkey -> Monkey -> Monkey -> Monkey
simplifyOp opMonkey lhs@(Leaf {}) rhs@(Leaf {})
  | name lhs == "humn" || name rhs == "humn" =
    opMonkey {lhs = name lhs, rhs = name rhs}
  | otherwise =
    Leaf
      {name = name opMonkey, value = ((getOp $ op opMonkey) `on` value) lhs rhs}
simplifyOp opMonkey _ _ = opMonkey

simplifyNode :: Monkey -> State (M.Map String Monkey) Monkey
simplifyNode m@(Leaf {}) = return m
simplifyNode opMonkey = do
  newLhs <- simplify $ lhs opMonkey
  newRhs <- simplify $ rhs opMonkey
  let result = (simplifyOp opMonkey newLhs newRhs)
  modify $ (\m -> M.insert (name opMonkey) result m)
  return $ result

simplify :: String -> State (M.Map String Monkey) Monkey
simplify name = do
  mappy <- get
  simplifyNode $ mappy ! name

runSimplification :: M.Map String Monkey -> M.Map String Monkey
runSimplification mappy = execState (simplify "root") mappy

maybeValue :: Monkey -> Maybe Integer
maybeValue Leaf{name="humn"} = Nothing
maybeValue Leaf{value=v} = Just v
maybeValue _ = Nothing

revOp :: Integer -> Maybe Integer -> Char -> Maybe Integer -> Integer
revOp result Nothing '+' (Just r) = result - r
revOp result (Just l) '+' Nothing = result - l
revOp result Nothing '-' (Just r) = result + r
revOp result (Just l) '-' Nothing = l - result
revOp result Nothing '*' (Just r) = result `div` r
revOp result (Just l) '*' Nothing = result `div` l
revOp result Nothing '/' (Just r) = result * r
revOp result (Just l) '/' Nothing = l `div` result

index :: Ord k => k -> M.Map k v -> v
index key mappy = mappy ! key

findNeededOp ::  M.Map String Monkey -> Monkey -> Integer -> Maybe Integer 
findNeededOp mappy monkey result = do
    let lhsMonkey = maybeValue $ mappy ! lhs monkey
    let rhsMonkey = maybeValue $ mappy ! rhs monkey
    let neededResult = revOp result lhsMonkey (op monkey) rhsMonkey
    case lhsMonkey of
        Nothing -> findNeeded mappy (lhs monkey) neededResult
        _ -> findNeeded mappy (rhs monkey) neededResult


findNeeded :: M.Map String Monkey -> String -> Integer -> Maybe Integer
findNeeded _ "humn" result = Just result
findNeeded mappy name result = do
    case mappy ! name of
        Leaf {} -> Nothing
        m@Node {} -> findNeededOp mappy m result


run :: IO ()
run = do
  input <- parseAllMonkeys <$> T.pack <$> readFile "inputs/dec21.txt"
  print $ evalMonkey input "root"
  let root = input ! "root"
  print $ dependsOn input "humn" $ lhs root
  print $ dependsOn input "humn" $ rhs root
  let simplified = (M.adjust (\m -> m{op='-'}) "root" $ runSimplification input)
  print $ simplified ! "root"
  print $ simplified ! rhs (simplified ! lhs (simplified ! "root"))
  print $ findNeeded simplified "root" 0
  -- let input2 = M.adjust (\m -> m {op = '-'}) "root" input 
  -- print $ head $ filter (tryNumber input2) [0..50000]
