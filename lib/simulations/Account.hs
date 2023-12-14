module Account where

import Market (Price, Ticker)

type Amount = Double

data Asset
  = Asset
      { aTicker :: Ticker
      , aBalance :: Amount
      , aPrice :: Price
      }
  | NotStaked

data Contract
  = Contract
      { cBtc :: Asset
      , cIusd :: Asset
      , cTrades :: [Trade]
      }
  | Unsubscribed

data Trade = Trade
  { tFrom :: Ticker
  , tTo :: Ticker
  , tPrice :: Price
  }

type AccountId = Double

data Account = Account
  { acId :: AccountId
  , acContract :: Contract
  , acStake :: Asset
  }
