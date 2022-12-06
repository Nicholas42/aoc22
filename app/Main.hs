module Main where

import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (getCurrentTime, utctDay)
import qualified Dec01 (run)
import qualified Dec02 (run)
import qualified Dec03 (run)
import qualified Dec04 (run)
import qualified Dec05 (run)
import qualified Dec06 (run)
import qualified Dec07 (run)
import qualified Dec08 (run)
import qualified Dec09 (run)
import qualified Dec10 (run)
import qualified Dec11 (run)
import qualified Dec12 (run)
import qualified Dec13 (run)
import qualified Dec14 (run)
import qualified Dec15 (run)
import qualified Dec16 (run)
import qualified Dec17 (run)
import qualified Dec18 (run)
import qualified Dec19 (run)
import qualified Dec20 (run)
import qualified Dec21 (run)
import qualified Dec22 (run)
import qualified Dec23 (run)
import qualified Dec24 (run)
import qualified Dec25 (run)
import System.Environment (getArgs)

getCurrentDate :: IO Int
getCurrentDate = do
  (_, _, day) <- getCurrentTime >>= return . toGregorian . utctDay
  return day

main :: IO ()
main = do
  args <- getArgs
  today <- getCurrentDate
  let date
        | null args = today
        | otherwise = read $ head args :: Int
  case date of
    1 -> Dec01.run
    2 -> Dec02.run
    3 -> Dec03.run
    4 -> Dec04.run
    5 -> Dec05.run
    6 -> Dec06.run
    7 -> Dec07.run
    8 -> Dec08.run
    9 -> Dec09.run
    10 -> Dec10.run
    11 -> Dec11.run
    12 -> Dec12.run
    13 -> Dec13.run
    14 -> Dec14.run
    15 -> Dec15.run
    16 -> Dec16.run
    17 -> Dec17.run
    18 -> Dec18.run
    19 -> Dec19.run
    20 -> Dec20.run
    21 -> Dec21.run
    22 -> Dec22.run
    23 -> Dec23.run
    24 -> Dec24.run
    25 -> Dec25.run
