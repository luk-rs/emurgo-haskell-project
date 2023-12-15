module Asset where

import Book (Price)
import Ticker (Ticker)

type Amount = Double

data Asset
  = Asset
      { aTicker :: Ticker
      , aBalance :: Amount
      , aPrice :: Price
      }
  | NotStaked