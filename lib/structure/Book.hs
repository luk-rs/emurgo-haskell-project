module Book where

import System.Random (randomRIO)
import Ticker (Ticker (ADA, BTC), priceFor)

type Price = Double

data Book = Book
  { bBtc :: Price
  , bAda :: Price
  }

bookFromRandom :: IO Book
bookFromRandom = do
  randomBtc <- randomRIO (16512.23, 43981.76)
  randomAda <- randomRIO (0.2234415, 0.5932197)
  return
    Book
      { bBtc = randomBtc
      , bAda = randomAda
      }

bookFromInput :: IO Book
bookFromInput = do
  btcPrice <- priceFor BTC
  adaPrice <- priceFor ADA
  return
    Book
      { bBtc = btcPrice
      , bAda = adaPrice
      }