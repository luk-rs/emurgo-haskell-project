module Contract where

import Asset (Asset)
import Trade (Trade)

data Contract
  = Contract
      { cBtc :: Asset
      , cIusd :: Asset
      , cTrades :: [Trade]
      }
  | Unsubscribed