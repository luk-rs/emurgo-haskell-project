module Sifo where

import Control.Monad.Reader
import Control.Monad.State
import System.Random (randomRIO)
import Text.Read (readMaybe)

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

randDouble :: Double -> Double -> IO Double
randDouble start end = randomRIO (start, end)

bookFromRandom :: IO Book
bookFromRandom = do
  randomBtc <- randDouble 16512.23 43981.76
  randomAda <- randDouble 0.2234415 0.5932197
  return
    Book
      { bBtc = randomBtc
      , bAda = randomAda
      }

readDouble :: IO (Maybe Double)
readDouble = readMaybe <$> getLine

data Ticker = BTC | ADA | IUSD deriving (Show)
priceFor :: Ticker -> IO Double
priceFor ticker = do
  putStrLn $ "Please insert $USD value for the following ticker =>" ++ show ticker
  putStr "$usd value :\\"
  maybe <- readDouble
  case maybe of
    Just price -> return price
    Nothing -> priceFor ticker

bookFromInput :: IO Book
bookFromInput = do
  btcPrice <- priceFor BTC
  adaPrice <- priceFor ADA
  return
    Book
      { bBtc = btcPrice
      , bAda = adaPrice
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
