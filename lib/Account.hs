module Account where

import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)

import Market (Market, Price, Ticker)

type Simulation a = ReaderT Market (StateT Account IO) a

data Account = Account
  { acId :: AccountId
  , acContract :: Contract
  , acStake :: Asset
  }

emptyAccount :: Account
emptyAccount =
  Account
    { acId = 231.231
    , acContract = Unsubscribed
    , acStake = NotStaked
    }

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
