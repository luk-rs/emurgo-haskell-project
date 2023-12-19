module Trade where

import Asset (Amount)
import Book (Price)
import Ticker (Ticker)

data Trade = Trade
  { tFrom :: Ticker
  , tTo :: Ticker
  , tPrice :: Price
  , tAmount :: Amount
  }