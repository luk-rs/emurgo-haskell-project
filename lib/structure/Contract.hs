module Contract where

import Asset (Amount)
import Book (Price)
import Trade (Trade)

type Beta = Int
type Trigger = Double

data Contract
  = Contract
      { cBtc :: Amount
      , cIusd :: Amount
      , cBeta :: Beta
      , cTrigger :: Trigger
      , cMeanBtcPrice :: Price
      }
  | Unsubscribed
