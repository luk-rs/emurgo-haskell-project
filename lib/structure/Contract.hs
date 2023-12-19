module Contract where

import Asset (Amount)
import Trade (Trade)

data Contract
  = Contract
      { cBtc :: Amount
      , cIusd :: Amount
      , cTrades :: [Trade]
      }
  | Unsubscribed