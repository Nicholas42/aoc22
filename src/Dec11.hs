{-# LANGUAGE OverloadedStrings #-}

module Dec11
  ( run
  ) where

import Control.Applicative ((*>), (<*), (<|>))
import Control.Monad.State (State, execState, get, modify, put)
import Data.Attoparsec.Combinator (many1)
import qualified Data.Attoparsec.Text as Atto
import Data.Attoparsec.Text ((<?>))
import Data.Either (fromLeft, isLeft)
import Data.List (sort)
import Data.Text (Text, pack, unpack)

data Monkey =
  Monkey
    { index :: Int
    , starting :: [Integer]
    , operation :: (Integer -> Integer)
    , test :: Test
    , inspectionNr :: Integer
    }

data Test =
  Test
    { divisor :: Integer
    , trueTarget :: Int
    , falseTarget :: Int
    }

data Throw =
  Throw
    { sourceMonkey :: Monkey
    , target :: Int
    , item :: Integer
    }

data Stat =
  Stat
    { monkeys :: [Monkey]
    , activeMonkey :: Int
    , roundNr :: Int
    , worryOperation :: (Integer -> Integer)
    }

type MonkeyState = State Stat ()

(!!!) :: [a] -> Int -> Maybe a
(!!!) list index
  | index < 0 || index >= length list = Nothing
  | otherwise = Just $ list !! index

commaSep :: Atto.Parser a -> Atto.Parser [a]
commaSep parser = Atto.sepBy parser (", ")

word :: Text -> Atto.Parser Text
word text = Atto.string text <* Atto.skipSpace

monkeyParser :: Atto.Parser Monkey
monkeyParser = do
  word "Monkey"
  number <- Atto.decimal <* Atto.char ':' <* Atto.endOfLine
  starting <- startingParser <?> "starting"
  operation <- operationParser <?> "operation"
  test <- testParser <?> "test"
  return $ Monkey number starting operation test 0

startingParser :: Atto.Parser [Integer]
startingParser = do
  Atto.skipSpace *> word "Starting items:"
  commaSep Atto.decimal <* Atto.endOfLine

makeOperation :: Either Integer Text -> Char -> (Integer -> Integer)
makeOperation (Left amount) c =
  case c of
    '*' -> (*) amount
    '+' -> (+) amount
makeOperation (Right "old") c =
  case c of
    '*' -> (\x -> x * x)
    '+' -> (\x -> x + x)

operationParser :: Atto.Parser (Integer -> Integer)
operationParser = do
  Atto.skipSpace *> Atto.string "Operation: new = old "
  operation <- ((Atto.char '+') <|> (Atto.char '*')) <* Atto.skipSpace
  amount <- (Atto.eitherP Atto.decimal $ Atto.string "old")
  (return $ makeOperation amount operation) <* Atto.endOfLine

testParser :: Atto.Parser Test
testParser = do
  Atto.skipSpace *> word "Test: divisible by"
  by <- Atto.decimal <* Atto.endOfLine
  trueTarget <- branchParser "true"
  falseTarget <- branchParser "false"
  return $ Test by trueTarget falseTarget

branchParser :: Text -> Atto.Parser Int
branchParser keyword = do
  Atto.skipSpace *> word "If" *> word keyword *> word ": throw to monkey"
  Atto.decimal <* Atto.endOfLine

monkeysParser :: Atto.Parser [Monkey]
monkeysParser = many1 (monkeyParser <* Atto.skipSpace) <* Atto.endOfInput

endRound :: State Stat ()
endRound = do
  modify (\stat -> stat {activeMonkey = 0, roundNr = 1 + roundNr stat})

replaceItem :: [a] -> Int -> a -> [a]
replaceItem list index item =
  (take index list) ++ item : (drop (index + 1) list)

transformItem :: [a] -> Int -> (a -> a) -> [a]
transformItem list index func = replaceItem list index (func $ list !! index)

inspect :: (Integer -> Integer) -> Monkey -> Monkey
inspect worryOp m =
  m
    { starting = transformItem (starting m) 0 $ worryOp . (operation m)
    , inspectionNr = 1 + inspectionNr m
    }

findThrow :: Monkey -> Throw
findThrow m = do
  let worry = head $ starting m
  let target =
        if mod worry (divisor $ test m) == 0
          then (trueTarget $ test m)
          else (falseTarget $ test m)
  Throw (m) target worry

executeThrow :: Throw -> State Stat ()
executeThrow t = do
  stat <- get
  let newMonkeys =
        transformItem
          (replaceItem
             (monkeys stat)
             (index $ sourceMonkey t)
             ((\m -> m {starting = tail $ starting m}) (sourceMonkey t)))
          (target t)
          (\m -> m {starting = starting m ++ [item t]})
  put stat {monkeys = newMonkeys}

monkeyStep :: State Stat ()
monkeyStep = do
  stat <- get
  let monkey = monkeys stat !! activeMonkey stat
  if null $ starting monkey
    then put $ stat {activeMonkey = 1 + activeMonkey stat}
    else executeThrow $ findThrow $ inspect (worryOperation stat) monkey

findModder :: [Monkey] -> Integer
findModder monkeys = do
  let divisors = map (divisor . test) monkeys
  foldl lcm 1 divisors

executeStep :: MonkeyState
executeStep = do
  stat <- get
  if length (monkeys stat) > activeMonkey stat
    then monkeyStep
    else endRound

runXRounds :: Int -> MonkeyState
runXRounds x = do
  stat <- get
  if (roundNr stat) >= x
    then return ()
    else executeStep >> runXRounds x

runSim :: [Monkey] -> [Monkey]
runSim ms = do
  let stat = Stat ms 0 0 (\x -> div x 3)
  monkeys $ execState (runXRounds 20) stat

runSim2 :: [Monkey] -> [Monkey]
runSim2 ms = do
  let stat = Stat ms 0 0 (\x -> mod x $ findModder ms)
  monkeys $ execState (runXRounds 10000) stat

evalScore :: [Monkey] -> Integer
evalScore monkeys = do
  let sorted = reverse $ sort $ map inspectionNr monkeys
  product $ take 2 sorted

runParts :: [Monkey] -> IO ()
runParts monkeys = do
  print $ evalScore $ runSim monkeys
  print $ evalScore $ runSim2 monkeys

run :: IO ()
run = do
  input <- readFile "inputs/dec11.txt"
  let result = Atto.parseOnly monkeysParser $ pack input
  either (print) (runParts) result
