module Sifo where

import Control.Concurrent (threadDelay)
import Control.Monad.RWS (MonadState (get), asks, foldM_, forM, forM_, gets, modify, unless, void, when)
import Control.Monad.Reader (MonadIO (liftIO))
import GHC.IO.Handle (hWaitForInput)
import Generics (ErrorMessage, handlingError, pressAnyKey)
import System.Console.ANSI (clearScreen)
import System.IO (stdin)
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
  subscribeContract 80 5 BTC amount subscriptionBook
  foldM_ (\prev iteration -> runIteration prev) subscriptionBook ([0 .. iterations] :: [Int])

runIteration :: Book -> Simulation Book
runIteration prevBook = do
  _ <- liftIO $ hWaitForInput stdin 500000
  market <- asks randomizeBookIO
  newBook <- liftIO $ market prevBook
  liftIO $ do
    putStrLn "New book"
    putStrLn $ "(BTC, IUSD) => (" <> show (bBtc newBook) <> ", " <> show (bAda newBook) <> ")"
    putStrLn ""
  contract <- gets acContract
  let c0 = cIusd contract
      q0 = cBtc contract
      p1 = bBtc newBook
      quocient = c0 / (q0 * p1)
      beta = cBeta contract
      trigger = cTrigger contract
      beta' = fromIntegral beta
      quocient' = ((100 - beta') / beta') * (1 / (1 + (trigger / 100)))
      shouldSellBtc = quocient < quocient'
  when shouldSellBtc $ sellBtc quocient' newBook
  let quocient'' = ((100 - beta') / beta') * (1 / (1 - (trigger / 100)))
      shouldBuyBtc = quocient > quocient''
  when shouldBuyBtc $ buyBtc quocient'' newBook
  return newBook

buyBtc :: Double -> Book -> Simulation ()
buyBtc quocient'' book = do
  contract <- gets acContract
  let c0 = cIusd contract
      q0 = cBtc contract
      p1 = bBtc book
      beta = cBeta contract
      trigger = cTrigger contract
      q2 = c0 - quocient'' * q0 * p1
      q1' = (100 - fromIntegral beta) / fromIntegral beta
      q3 = (1 + q1') * p1
      btcBuy = q2 / q3
      iusd' = c0 - btcBuy * p1
      cBtc' = q0 + btcBuy
      meanPrice = (q0 * cMeanBtcPrice contract + btcBuy * p1) / cBtc'
  modify $ \account -> account{acContract = contract{cBtc = cBtc', cIusd = iusd', cMeanBtcPrice = meanPrice}}
  liftIO $ do
    putStrLn "Buying BTC"
    putStrLn $ "(BTC, IUSD) => (" <> show q0 <> ", " <> show c0 <> ") => (" <> show cBtc' <> ", " <> show iusd' <> ")"
    putStrLn $ "Mean Price " <> show meanPrice
    putStrLn ""
  return ()

sellBtc :: Double -> Book -> Simulation ()
sellBtc quocient' book = do
  contract <- gets acContract
  let c0 = cIusd contract
      q0 = cBtc contract
      p1 = bBtc book
      beta = cBeta contract
      trigger = cTrigger contract
      q2 = quocient' * q0 * p1 - c0
      q1' = (100 - fromIntegral beta) / fromIntegral beta
      q3 = (1 + q1') * p1
      btcSell = q2 / q3
      iusd' = c0 + btcSell * p1
      cBtc' = q0 - btcSell
  modify $ \account -> account{acContract = contract{cBtc = cBtc', cIusd = iusd'}}
  liftIO $ do
    putStrLn "Selling BTC"
    putStrLn $ "(BTC, IUSD) => (" <> show q0 <> ", " <> show c0 <> ") => (" <> show cBtc' <> ", " <> show iusd' <> ")"
    putStrLn $ "Mean Price " <> show (cMeanBtcPrice contract)
    putStrLn ""
  return ()

subscribeContract :: Beta -> Trigger -> Ticker -> Amount -> Book -> Simulation ()
subscribeContract beta trigger ticker amount book = do
  let crypto = amount * fromIntegral beta / 100 / bBtc book
      stable = amount * (100 - fromIntegral beta) / 100
  modify $ \account ->
    account
      { acStake = Asset{aTicker = ticker, aPrice = bBtc book, aBalance = amount}
      , acContract = Contract{cBtc = crypto, cIusd = stable, cBeta = beta, cTrigger = trigger / 100, cMeanBtcPrice = bBtc book}
      }
  liftIO $ do
    putStrLn "Subscribed"
    putStrLn $ "(BTC, IUSD) => (" <> show crypto <> ", " <> show stable <> ")"
    putStrLn $ "Entry Price " <> show (bBtc book)
    putStrLn ""

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

selectIterations :: ErrorMessage -> IO Int
selectIterations error = do
  handlingError error
  putStrLn "How many iterations you wish the simulation to run for:"
  maybe <- readMaybe <$> getLine
  case maybe of
    Nothing -> selectIterations "THAT IS NOT A VALID NUMBER, PLEASE INSERT DIGITS REPRESENTING THE AMOUNT OF ITERATIONS YOU WISH TO SIMULATE"
    Just weeks -> return weeks
