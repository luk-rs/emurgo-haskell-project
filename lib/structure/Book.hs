module Book where

import System.Random (randomRIO)
import Ticker (Ticker (ADA, BTC), priceFor)

type Price = Double

data Book = Book
  { bTicker :: Ticker
  , bPrice :: Price
  }

bookFromRandom :: Ticker -> (Price, Price) -> IO Book
bookFromRandom ticker range = do
  randomBtc <- randomRIO range
  return
    Book
      { bTicker = ticker
      , bPrice = randomBtc
      }

bookFromInput :: IO Book
bookFromInput = undefined