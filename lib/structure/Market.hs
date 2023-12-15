module Market where

import Book (Book (..), bookFromInput, bookFromRandom)
import System.Random (randomRIO)
import Text.Read (readMaybe)

data Market = Market
  { randomizeBookIO :: IO Book
  , manualBookIO :: IO Book
  }

singleMarket :: Market
singleMarket =
  Market
    { randomizeBookIO = bookFromRandom
    , manualBookIO = bookFromInput
    }
