module Market where

import System.Random (randomRIO)
import Text.Read (readMaybe)

type Price = Double

data Ticker = BTC | ADA | IUSD deriving (Show)

data Book = Book
  { bBtc :: Price
  , bAda :: Price
  }

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