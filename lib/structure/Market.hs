module Market where

import Book (Book (..), Price, bookFromRandom)
import System.Random (randomRIO)
import Text.Read (readMaybe)
import Ticker (Ticker)

data Market = Market
  { randomizeBookIO :: Book -> IO Book
  }

singleMarket :: Market
singleMarket =
  Market
    { randomizeBookIO = bookFromRandom
    }
