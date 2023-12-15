module Trade where

import Book (Price)
import Ticker (Ticker)

data Trade = Trade
  { tFrom :: Ticker
  , tTo :: Ticker
  , tPrice :: Price
  }