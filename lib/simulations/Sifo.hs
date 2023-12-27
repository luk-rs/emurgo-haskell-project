module Sifo where

import Control.Concurrent (threadDelay)
import Control.Monad.RWS (MonadState (get), asks, foldM_, forM, forM_, gets, modify, unless, void, when)
import Control.Monad.Reader (MonadIO (liftIO))
import GHC.IO.Handle (hWaitForInput)
import Generics (ErrorMessage, handlingError, pressAnyKey)
import System.Console.ANSI (clearScreen)
import System.IO (stdin, stdout)
import System.Random (randomRIO)
import Text.Read (readMaybe)

import Account (Account (..))
import Asset (Amount, Asset (..))
import Book (Book (..), Price, initialBook)
import Contract (Beta, Contract (..), Trigger)
import Control.Monad.Cont (cont)
import Market (Market (randomizeBookIO))
import Navigation (Navigation (Back, Forward))
import Simulation (Simulation)
import Ticker (Ticker (..), chooseTicker)
import Trade (Trade (..))

autoSifo :: Simulation Navigation
autoSifo = do
  -- ticker <- liftIO (chooseSifoTicker "")
  let ticker = IUSD
  -- amount <- liftIO (selectAmountToStake ticker "")
  let amount = 1000
  -- range@(low, high) <- liftIO (selectRangeForTicker ticker)
  iterations <- liftIO (selectIterations "")
  runSimulation ticker amount iterations
  return $ Forward 3

randPrice :: (Price, Price) -> IO Price
randPrice = randomRIO

runSimulation :: Ticker -> Amount -> Int -> Simulation ()
runSimulation ticker amount iterations = do
  subscriptionBook <-
    liftIO
      $ clearScreen
      >> putStrLn ("SIMULATING " <> show amount <> "$ of $" <> show ticker <> " FOR " <> show iterations <> " iterations")
      >> initialBook
  subscribeContract 80 15 BTC amount subscriptionBook
  foldM_ (\prev iteration -> runIteration prev) subscriptionBook ([1 .. iterations] :: [Int])

subscribeContract :: Beta -> Trigger -> Ticker -> Amount -> Book -> Simulation ()
subscribeContract beta trigger ticker amount (Book p1 _) = do
  let crypto = amount * beta / 100 / p1
      stable = amount * (100 - beta) / 100
  modify $ \account ->
    account
      { acStake = Asset{aTicker = ticker, aPrice = p1, aBalance = amount}
      , acContract = Contract{cBtc = crypto, cIusd = stable, cBeta = beta, cTrigger = trigger / 100, cMeanBtcPrice = p1}
      }
  liftIO $ printSubscription crypto stable p1

runIteration :: Book -> Simulation Book
runIteration prevBook = do
  liftIO $ hWaitForInput stdin 500
  market <- asks randomizeBookIO
  newBook@(Book p1 _) <- liftIO $ market prevBook
  contract@(Contract q0 c0 beta trigger _) <- gets acContract
  liftIO $ printBook prevBook newBook trigger
  let proportion = c0 / (q0 * p1)
      q0p1 = q0 * p1
      betaC0 = beta * c0
      stable = 100 - beta
      price = 100 * p1
      sellRatio = (stable * q0p1 - betaC0) / price
      shouldSellBtc = proportion < sellRatio
  liftIO . putStrLn $ "SELL >> (proprtion) " <> show proportion <> " < " <> show sellRatio <> "(ratio) = " <> show shouldSellBtc
  when shouldSellBtc $ sellBtc contract sellRatio newBook
  let buyRatio = (betaC0 - stable * q0p1) / price
      shouldBuyBtc = proportion > buyRatio
  liftIO . putStrLn $ "BUY >> (proprtion) " <> show proportion <> " > " <> show buyRatio <> "(ratio) = " <> show shouldBuyBtc
  when shouldBuyBtc $ buyBtc contract buyRatio newBook
  return newBook

buyBtc :: Contract -> Double -> Book -> Simulation ()
buyBtc (Contract q0 c0 beta trigger mean) buyRatio book@(Book p1 _) = do
  contract <- gets acContract
  let q2 = c0 - buyRatio * q0 * p1
      q1' = (100 - beta) / beta
      q3 = (1 + q1') * p1
      btcBuy = q2 / q3
      iusd' = c0 - btcBuy * p1
      cBtc' = q0 + btcBuy
      meanPrice = (q0 * mean + btcBuy * p1) / cBtc'
  modify $ \account -> account{acContract = contract{cBtc = cBtc', cIusd = iusd', cMeanBtcPrice = meanPrice}}
  printTrade "Buying BTC" contract book

sellBtc :: Contract -> Double -> Book -> Simulation ()
sellBtc (Contract q0 c0 beta trigger mean) sellRatio book@(Book p1 _) = do
  contract <- gets acContract
  let q2 = sellRatio * q0 * p1 - c0
      q1' = (100 - beta) / beta
      q3 = (1 + q1') * p1
      btcSell = q2 / q3
      iusd' = c0 + btcSell * p1
      cBtc' = q0 - btcSell
  modify $ \account -> account{acContract = contract{cBtc = cBtc', cIusd = iusd'}}
  printTrade "Selling BTC" contract book

printBook :: Book -> Book -> Double -> IO ()
printBook (Book p0 _) (Book p1 _) trigger = do
  putStrLn "New book"
  putStrLn $ "BTC ( cur <- prev )\n(" <> show p1 <> " <- " <> show p0 <> ")"
  let fluctuation = (p1 - p0) / (p1 + p0) * 100
  putStrLn $ "Fluctuation " <> show fluctuation
  putStrLn $ "Trigger " <> show (trigger * 100)
  putStrLn ""

printTrade :: String -> Contract -> Book -> Simulation ()
printTrade message (Contract q0 c0 _ _ mean) (Book p1 _) = do
  (Contract cBtc iUsd _ _ _) <- gets acContract
  liftIO $ do
    putStrLn message
    putStrLn $ "(BTC, IUSD) => (" <> show q0 <> ", " <> show c0 <> ") => (" <> show cBtc <> ", " <> show iUsd <> ")"
    let iUsdBtc = iUsd / p1
        totalBtc = cBtc + iUsdBtc
    putStrLn $ "Total BTC " <> show totalBtc
    let btcIusd = cBtc * p1
        totalIusd = iUsd + btcIusd
    putStrLn $ "Total USD " <> show totalIusd
    let pctgIusd = iUsd / btcIusd * 100
        pctgBtc = iUsdBtc / cBtc * 100
    putStrLn $ "% " <> show pctgIusd <> " | " <> show pctgBtc
    putStrLn $ "$BTC " <> show p1
    putStrLn $ "Mean Price " <> show mean
    putStrLn ""

printSubscription :: Double -> Double -> Price -> IO ()
printSubscription crypto stable price = do
  putStrLn "Subscribed"
  putStrLn $ "(BTC, IUSD) => (" <> show crypto <> ", " <> show stable <> ")"
  putStrLn $ "Entry Price " <> show price
  putStrLn ""

-- chooseSifoTicker :: ErrorMessage -> IO Ticker
-- chooseSifoTicker error = do
--   handlingError error
--   putStrLn "Please choose a $Ticker to simulate rebalancing strategy:"
--   maybe <- chooseTicker
--   case maybe of
--     Nothing -> chooseSifoTicker "INVALID OPTION, PLEASE CHOOSE A VALID NUMBER"
--     Just ticker -> return ticker

-- selectAmountToStake :: Ticker -> ErrorMessage -> IO Double
-- selectAmountToStake ticker error = do
--   handlingError error
--   putStrLn "Insert the amount you're wish to stake:"
--   maybe <- readMaybe <$> getLine
--   case maybe of
--     Nothing -> selectAmountToStake ticker "THAT IS NOT A VALID AMOUNT, IT MUST BE IN FORMAT '6.9' WITH THE VALUE YOU DESIRE"
--     Just amount -> return amount

selectIterations :: ErrorMessage -> IO Int
selectIterations error = do
  handlingError error
  putStrLn "How many iterations you wish the simulation to run for:"
  maybe <- readMaybe <$> getLine
  case maybe of
    Nothing -> selectIterations "THAT IS NOT A VALID NUMBER, PLEASE INSERT DIGITS REPRESENTING THE AMOUNT OF ITERATIONS YOU WISH TO SIMULATE"
    Just weeks -> return weeks
