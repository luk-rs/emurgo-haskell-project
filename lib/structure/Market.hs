module Market where

import Book (Book (..), Price, bookFromInput, bookFromRandom)
import System.Random (randomRIO)
import Text.Read (readMaybe)
import Ticker (Ticker)

data Market = Market
  { randomizeBookIO :: Ticker -> (Price, Price) -> IO Book
  , manualBookIO :: IO Book
  }

singleMarket :: Market
singleMarket =
  Market
    { randomizeBookIO = bookFromRandom
    , manualBookIO = bookFromInput
    }
