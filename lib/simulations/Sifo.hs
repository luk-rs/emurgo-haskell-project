module Sifo where

import Control.Concurrent (threadDelay)
import Control.Monad.RWS (MonadState (get), asks, foldM_, forM, forM_, gets, modify, modify', unless, void, when)
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

autoSifo :: Beta -> Simulation ()
autoSifo beta = do
  let ticker = IUSD
  amount <- liftIO $ selectAmountToStake ticker ""
  trigger <- liftIO $ selectTrigger ""
  iterations <- liftIO (selectIterations "")
  runSimulation beta ticker amount iterations

runSimulation :: Beta -> Ticker -> Amount -> Int -> Simulation ()
runSimulation beta ticker amount iterations = do
  subscriptionBook <-
    liftIO
      $ do
        clearScreen >> putStr "\r"
        putStrLn ("SIMULATING " <> show amount <> "$ of $" <> show ticker <> " FOR " <> show iterations <> " iterations")
        initialBook
  subscribeContract beta 0.05 BTC amount subscriptionBook
  foldM_ (\prev iteration -> runIteration prev) subscriptionBook ([1 .. iterations] :: [Int])

subscribeContract :: Beta -> Trigger -> Ticker -> Amount -> Book -> Simulation ()
subscribeContract beta trigger ticker amount (Book p1 _) = do
  let crypto = amount * beta / 100 / p1
      stable = amount * (100 - beta) / 100
  modify $ \account ->
    let subscription =
          Contract
            { cBtc = crypto
            , cIusd = stable
            , cBeta = beta
            , cTrigger = trigger / 100
            , cMeanBtcPrice = p1
            , cMaxPrice = p1
            , cMinPrice = p1
            }
     in account
          { acStake = Asset{aTicker = ticker, aPrice = p1, aBalance = amount}
          , acContract = subscription
          }
  liftIO $ printSubscription crypto stable p1

runIteration :: Book -> Simulation Book
runIteration prevBook@(Book p0 _) = do
  -- liftIO $ hWaitForInput stdin 500
  market <- asks randomizeBookIO
  newBook@(Book p1 _) <- liftIO $ market prevBook
  contract@(Contract q0 c0 beta a _ _ _) <- gets acContract
  let q0p1 = q0 * p1
      betaC0 = beta * c0
      pStable = 100 - beta
      p1' = 100 * p1
  let sellTrigger = p1 > (1 + a / 100) * p0
      buyTrigger = p1 < (1 - a / 100) * p0
  if sellTrigger
    then do
      let q' = (pStable * q0p1 - betaC0) / p1'
      sellBtc q' newBook
    else when buyTrigger $ do
      let q' = (betaC0 - pStable * q0p1) / p1'
      buyBtc q' newBook
  updateBoundaries newBook
  return newBook

updateBoundaries :: Book -> Simulation ()
updateBoundaries (Book p' _) = do
  contract@(Contract _ _ _ _ _ max min) <- gets acContract
  modify $ \account ->
    account
      { acContract =
          contract
            { cMaxPrice = if p' > max then p' else max
            , cMinPrice = if p' < min then p' else min
            }
      }
  return ()

buyBtc :: Double -> Book -> Simulation ()
buyBtc q' book@(Book p1 _) = do
  contract@(Contract q0 c0 _ _ pm0 _ _) <- gets acContract
  let q1 = q0 + q'
      c1 = c0 - q' * p1
      pm1 = (pm0 * q0 + p1 * q') / q1
  modify $ \account -> account{acContract = contract{cBtc = q1, cIusd = c1, cMeanBtcPrice = pm1}}
  contract@(Contract q0 c0 _ _ _ _ _) <- gets acContract
  printTrade "Buying BTC" contract book

sellBtc :: Double -> Book -> Simulation ()
sellBtc q' book@(Book p1 _) = do
  contract@(Contract q0 c0 _ _ _ _ _) <- gets acContract
  let q1 = q0 - q'
      c1 = c0 + q' * p1
  modify $ \account -> account{acContract = contract{cBtc = q1, cIusd = c1}}
  contract@(Contract q0 c0 _ _ _ _ _) <- gets acContract
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
printTrade message (Contract q0 c0 _ _ pm0 _ _) (Book p1 _) = do
  (Contract q0 c0 _ _ _ _ _) <- gets acContract
  liftIO $ do
    putStrLn message
    putStrLn $ "(BTC, IUSD) => (" <> show q0 <> ", " <> show c0 <> ") => (" <> show q0 <> ", " <> show c0 <> ")"
    let iUsdBtc = c0 / p1
        totalBtc = q0 + iUsdBtc
    putStrLn $ "Total BTC " <> show totalBtc
    let btcIusd = q0 * p1
        totalIusd = c0 + btcIusd
    putStrLn $ "Total USD " <> show totalIusd
    let pctgIusd = c0 / btcIusd * 100
        pctgBtc = iUsdBtc / q0 * 100
    putStrLn $ "% " <> show pctgIusd <> " | " <> show pctgBtc
    putStrLn $ "$BTC " <> show p1
    putStrLn $ "Mean Price " <> show pm0
    putStrLn ""

printSubscription :: Double -> Double -> Price -> IO ()
printSubscription crypto stable price = do
  putStrLn "Subscribed"
  putStrLn $ "(BTC, IUSD) => (" <> show crypto <> ", " <> show stable <> ")"
  putStrLn $ "Entry Price " <> show price
  putStrLn ""

selectIterations :: ErrorMessage -> IO Int
selectIterations error = do
  handlingError error
  putStrLn "\rHow many iterations you wish the simulation to run for:"
  maybe <- readMaybe <$> getLine
  case maybe of
    Nothing -> selectIterations "\rTHAT IS NOT A VALID NUMBER, PLEASE INSERT DIGITS REPRESENTING THE AMOUNT OF ITERATIONS YOU WISH TO SIMULATE"
    Just weeks -> return weeks

selectAmountToStake :: Ticker -> ErrorMessage -> IO Double
selectAmountToStake ticker error = do
  handlingError error
  putStrLn "\rInsert the amount you're wish to stake:"
  maybe <- readMaybe <$> getLine
  case maybe of
    Nothing -> selectAmountToStake ticker "\rTHAT IS NOT A VALID AMOUNT, IT MUST BE IN FORMAT '6.9' WITH THE VALUE YOU DESIRE"
    Just amount -> return amount

selectTrigger :: ErrorMessage -> IO Double
selectTrigger error = do
  handlingError error
  putStrLn "\rChoose a value between 0 and 100. It's going to represent the % of value drift from the initial amount to rebalance the portfolio."
  maybe <- readMaybe <$> getLine
  case maybe of
    Nothing -> selectTrigger "\rTHAT IS NOT A VALID VALUE, IT MUST BE IN FORMAT '25' WITH THE VALUE YOU DESIRE"
    Just amount ->
      if amount > 100 || amount < 1
        then selectTrigger "\rIT SHOULD BE A NUMBER IN THE RANGE [ 1 - 100 ]"
        else return amount