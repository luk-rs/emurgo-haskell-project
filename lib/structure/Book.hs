module Book where

import System.Random (randomIO, randomRIO)
import Ticker (Ticker (ADA, BTC), priceFor)

-- import Data.String.CSV
import Text.ParserCombinators.Parsec (parseFromFile)

type Price = Double

data Book
  = Book
      { bBtc :: Price
      , bAda :: Price
      }
  | CsvBook
      { bBtc :: Price
      , bAda :: Price
      , bCsv :: [Price]
      }

randomizePrice :: Price -> IO Price
randomizePrice prev = do
  toss <- randomRIO (0, 100)
  let up = (toss :: Int) < 55
  percentage <-
    if up
      then randomRIO (0, 7)
      else
        if toss > 99
          then randomRIO (7, 20)
          else randomRIO (0, 7)
  let change = prev * percentage / 100
  putStrLn $ "TOSS: " <> show toss
  return
    $ if up
      then prev + change
      else prev - change

bookFromRandom :: Book -> IO Book
bookFromRandom book = do
  let prevBtc = bBtc book
  randomBtc <- randomizePrice prevBtc
  let prevAda = bAda book
  randomAda <- randomizePrice prevAda
  return
    Book
      { bBtc = randomBtc
      , bAda = randomAda
      }

initialBook :: IO Book
initialBook = do
  randomBtc <- randomRIO (25432.12, 28891.33)
  randomAda <- randomRIO (0.22432, 0.26763)
  return
    $ Book
      { bBtc = randomBtc
      , bAda = randomAda
      }
