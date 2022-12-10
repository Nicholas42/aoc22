module Dec10
  ( run
  ) where

data Instruction
  = Noop
  | Add Int
  deriving (Show)

readInstruction :: String -> Instruction
readInstruction line
  | line == "noop" = Noop
  | otherwise = Add $ read $ (words line) !! 1

repeatHead :: [a] -> [a]
repeatHead list = (head list) : list

executeInstruction :: [Int] -> Instruction -> [Int]
executeInstruction stack Noop = repeatHead stack
executeInstruction stack (Add v) = (v + head stack) : repeatHead stack

signalStrength :: [Int] -> Int -> Int
signalStrength stack cycle = cycle * stack !! (cycle - 1)

spriteCovers :: Int -> Int -> Char
spriteCovers pos pixel
  | pos - 1 <= realPixel && realPixel <= pos + 1 = '#'
  | otherwise = ' '
  where
    realPixel = mod pixel 40

drawScreen :: [Int] -> [Char]
drawScreen stack = zipWith spriteCovers stack [0 ..]

screenLines :: [Char] -> [[Char]]
screenLines [] = []
screenLines screen = (take 40 screen) : (screenLines $ drop 40 screen)

run :: IO ()
run = do
  result <-
    reverse <$> foldl executeInstruction [1] <$> map readInstruction <$> lines <$>
    readFile "inputs/dec10.txt"
  print $ sum $ map (signalStrength result) [20,60 .. length result]
  mapM_ putStrLn $ screenLines $ drawScreen result
