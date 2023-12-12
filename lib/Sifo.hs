module Sifo where

import Control.Monad.Reader
import Control.Monad.State

type Price = Double
type Amount = Double
type Id = Double

type Simulation a = ReaderT Market (StateT Account IO) a

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

bookFromRandom :: IO Book
bookFromRandom = do
  putStrLn "Book From Random"
  return
    Book
      { bBtc = 32145.23
      , bAda = 0.2532
      }

bookFromInput :: IO Book
bookFromInput = do
  putStrLn "Book From Input"
  return
    Book
      { bBtc = 32145.23
      , bAda = 0.2532
      }

data Book = Book
  { bBtc :: Price
  , bAda :: Price
  }

data Account = Account
  { acId :: Id
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

data Ticker = BTC | ADA | IUSD
data Asset
  = Asset
      { aTicker :: Ticker
      , aBalance :: Amount
      , aPrice :: Price
      }
  | NotStaked

data Trade = Trade
  { tFrom :: Ticker
  , tTo :: Ticker
  , tPrice :: Price
  }

data Contract
  = Contract
      { cBtc :: Asset
      , cIusd :: Asset
      , cTrades :: [Trade]
      }
  | Unsubscribed
