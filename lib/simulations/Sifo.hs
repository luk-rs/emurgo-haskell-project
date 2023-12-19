module Sifo where

import Control.Concurrent (threadDelay)
import Control.Monad.RWS (MonadState (get), asks, forM, forM_, gets, modify, unless, void)
import Control.Monad.Reader (MonadIO (liftIO))
import GHC.IO.FD (stdin)
import GHC.IO.Handle (hWaitForInput)
import Generics (ErrorMessage, handlingError, pressAnyKey)
import System.Console.ANSI (clearScreen)
import System.Random (randomRIO)
import Text.Read (readMaybe)

import Account (Account (..))
import Asset (Amount, Asset (..))
import Book (Book (..), Price)
import Contract (Contract (..))
import Market (Market (randomizeBookIO))
import Navigation (Navigation (Back, Forward))
import Simulation (Simulation)
import Ticker (Ticker (..), chooseTicker)
import Trade (Trade (..))

autoSifo :: Simulation Navigation
autoSifo = do
  ticker <- liftIO (chooseSifoTicker "")
  amount <- liftIO (selectAmountToStake ticker "")
  range@(low, high) <- liftIO (selectRangeForTicker ticker)
  weeks <- liftIO (selectWeeks "")
  runSimulation ticker amount range weeks
  return $ Forward 3

randPrice :: (Price, Price) -> IO Price
randPrice = randomRIO

runSimulation :: Ticker -> Amount -> (Price, Price) -> Int -> Simulation ()
runSimulation ticker amount range@(high, low) weeks = do
  liftIO
    $ clearScreen
    >> putStrLn ("GREAT, SIMULATING $" <> show ticker <> " FOR " <> show weeks <> " weeks between " <> show low <> "$-" <> show high <> "$")
  (subBook : rebBooks) <- generateBooks ticker range weeks
  subscribeContract ticker amount subBook
  forM_ rebBooks (rebalanceFromBook ticker)

subscribeContract :: Ticker -> Amount -> Book -> Simulation ()
subscribeContract ticker amount book = do
  let sixty = amount * 0.6
      fourty = amount * 0.4 * bPrice book
      subscriptionTrade = Trade{tFrom = ticker, tTo = IUSD, tPrice = bPrice book, tAmount = amount}
  modify $ \account ->
    account
      { acStake = Asset{aTicker = ticker, aPrice = bPrice book, aBalance = amount}
      , acContract = Contract{cBtc = sixty, cIusd = fourty, cTrades = [subscriptionTrade]}
      }

generateBooks :: Ticker -> (Price, Price) -> Int -> Simulation [Book]
generateBooks ticker range weeks = do
  marketRandomizer <- asks randomizeBookIO
  liftIO
    $ forM [1 .. weeks + 1]
    $ \week -> do
      threadDelay 25000
      book <- marketRandomizer ticker range
      liftIO . putStrLn $ "Randomized week " <> show week <> " for ticker $" <> show ticker <> " having price " <> show (bPrice book) <> "$"
      return book

rebalanceFromBook :: Ticker -> Book -> Simulation ()
rebalanceFromBook ticker book = do
  contract <- gets acContract
  let tickerAmount = cBtc contract
      stableAsTickerAmount = cIusd contract / bPrice book
      totalTickerAmount = tickerAmount + stableAsTickerAmount
      sixty = totalTickerAmount * 0.6
      fourty = totalTickerAmount * 0.4 * bPrice book
      (from, to, amount) =
        if sixty > tickerAmount
          then do
            -- I'm buying btc from iUsd
            let tickerAmountToBuy = sixty - tickerAmount
                iUsdAmountToSell = tickerAmountToBuy * bPrice book
            (IUSD, ticker, iUsdAmountToSell)
          else do
            -- I'm buying iUsd from btc
            let tickerAmountToSell = tickerAmount - sixty
            (ticker, IUSD, tickerAmountToSell)
      rebalanceTrade = Trade{tTo = to, tFrom = from, tPrice = bPrice book, tAmount = amount}
  modify $ \account -> do
    let contract = acContract account
        prevTrades = cTrades contract
    account{acContract = Contract{cBtc = sixty, cIusd = fourty, cTrades = rebalanceTrade : prevTrades}}
  liftIO . putStrLn $ "Selling from $" <> show (tFrom rebalanceTrade) <> " to $" <> show (tTo rebalanceTrade) <> " " <> show (tAmount rebalanceTrade) <> " tokens"

chooseSifoTicker :: ErrorMessage -> IO Ticker
chooseSifoTicker error = do
  handlingError error
  putStrLn "Please choose a $Ticker to simulate rebalancing strategy:"
  maybe <- chooseTicker
  case maybe of
    Nothing -> chooseSifoTicker "INVALID OPTION, PLEASE CHOOSE A VALID NUMBER"
    Just ticker -> return ticker

selectAmountToStake :: Ticker -> ErrorMessage -> IO Double
selectAmountToStake ticker error = do
  handlingError error
  putStrLn "Insert the amount you're wish to stake:"
  maybe <- readMaybe <$> getLine
  case maybe of
    Nothing -> selectAmountToStake ticker "THAT IS NOT A VALID AMOUNT, IT MUST BE IN FORMAT '6.9' WITH THE VALUE YOU DESIRE"
    Just amount -> return amount

selectRangeForTicker :: Ticker -> IO (Price, Price)
selectRangeForTicker ticker = do
  low <- selectCustomRange "lower" ticker ""
  high <- selectCustomRange "higher" ticker ""
  return (low, high)

selectCustomRange :: String -> Ticker -> ErrorMessage -> IO Price
selectCustomRange range ticker error = do
  handlingError error
  putStrLn $ "Please choose the " <> range <> " range price for $" <> show ticker <> " :"
  maybe <- readMaybe <$> getLine
  case maybe of
    Nothing -> selectCustomRange range ticker "PLEASE CHOOSE A VALID VALUE FOR THE LOWER RANGE, IT MUST BE IN FORMAT '6.9' WITH THE VALUE YOU DESIRE"
    Just lower -> return lower

selectWeeks :: ErrorMessage -> IO Int
selectWeeks error = do
  handlingError error
  putStrLn "How many weeks you wish the simulation to run for:"
  maybe <- readMaybe <$> getLine
  case maybe of
    Nothing -> selectWeeks "THAT IS NOT A VALID NUMBER, PLEASE INSERT DIGITS REPRESENTING THE AMOUNT OF WEEKS YOU WISH TO SIMULATE"
    Just weeks -> return weeks
