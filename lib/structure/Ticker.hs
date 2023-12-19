module Ticker where

import Control.Monad (forM_)
import Data.Map (Map, fromList, (!))
import GHC.Read (choose)
import Text.Read (readMaybe)

data Ticker = BTC | ADA | IUSD deriving (Show, Eq, Enum, Bounded)

priceFor :: Ticker -> IO Double
priceFor ticker = do
  putStrLn $ "Please insert $USD value for the following ticker =>" ++ show ticker
  putStr "$usd value :\\"
  maybe <- readMaybe <$> getLine
  case maybe of
    Just price -> return price
    Nothing -> priceFor ticker

chooseTicker :: IO (Maybe Ticker)
chooseTicker = do
  let tickers = [ADA, BTC]
  tickersMap <- printTickers tickers
  maybe <- readMaybe . flip (:) "" <$> getChar
  return $ case maybe of
    Nothing -> Nothing
    Just option -> Just $ tickersMap ! option

printTickers :: [Ticker] -> IO (Map Int Ticker)
printTickers tickers = do
  let options = zip [1 ..] tickers
  forM_ options $ \(idx, ticker) -> putStrLn ("\t" <> show idx <> ". " <> show ticker)
  return . fromList $ options
