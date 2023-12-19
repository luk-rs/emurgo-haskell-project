module Inspection where

import Control.Monad.Cont (cont)
import Control.Monad.RWS (MonadIO (liftIO), MonadState (get), forM_)
import GHC.IO.Handle (hFlush)
import System.Console.ANSI (clearScreen)

import Account (Account (..))
import Asset (Asset (..))
import Contract (Contract (..))
import Generics (pressAnyKey)
import Navigation (Navigation (..))
import Simulation (Simulation)
import System.Exit (exitFailure)
import Ticker (Ticker (..))
import Trade (Trade (..))

-- inspectTrades :: Simulation Navigation
-- inspectTrades = do
--   liftIO clearScreen
--   account <- get
--   let contract = acContract account
--       stake = acStake account
--   case contract of
--     Unsubscribed -> liftIO $ clearScreen >> putStrLn "Contract unsubscribed" >> pressAnyKey >> return (Forward 1)
--     contract@(Contract cBtc cIusd cTrades) -> liftIO (printStake stake >> forM_ (init cTrades) printTrade >> printFinalTickerValue contract >> pressAnyKey) >> return Stay

-- printFinalTickerValue :: Contract -> IO ()
-- printFinalTickerValue (Contract tickerAmount iusd trades) = do
--   let (Trade from to price _) = last trades
--       ticker = if from == IUSD then to else from
--       iUsdToTicker = iusd / price
--       totalTicker = tickerAmount + iUsdToTicker
--   putStrLn $ "Would you redeem and you would have " <> show totalTicker <> " tokens of $" <> show ticker
--   let transactionPrices = map tPrice trades
--       meanTransactionPrice = sum transactionPrices / fromIntegral (length transactionPrices)
--   putStrLn $ "\twith average transaction price " <> show meanTransactionPrice <> "$"
--   putStrLn ""

-- printStake :: Asset -> IO ()
-- printStake (Asset ticker balance price) = do
--   putStrLn $ "Aquired " <> show balance <> " tokens of $" <> show ticker
--   putStrLn $ "\twith price " <> show price <> "$"
--   putStrLn ""

-- printTrade :: Trade -> IO ()
-- printTrade (Trade from to price amount) = do
--   let worth = case from of
--         IUSD -> amount / price
--         _ -> amount * price
--   putStrLn $ "Sold " <> show amount <> " $" <> show from <> " into $" <> show to <> " with price " <> show price <> "$"
--   putStrLn $ "\tworthing about " <> show worth <> "$" <> show to
--   putStrLn ""
