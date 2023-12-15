module Ticker where

import Text.Read (readMaybe)

data Ticker = BTC | ADA | IUSD deriving (Show)

priceFor :: Ticker -> IO Double
priceFor ticker = do
  putStrLn $ "Please insert $USD value for the following ticker =>" ++ show ticker
  putStr "$usd value :\\"
  maybe <- readMaybe <$> getLine
  case maybe of
    Just price -> return price
    Nothing -> priceFor ticker