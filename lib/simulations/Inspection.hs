module Inspection where

import Account (Account (Account))
import Asset (Asset (Asset, aPrice))
import Contract (Contract (Contract, cBtc, cMeanBtcPrice))
import Control.Monad.RWS (MonadIO (liftIO), MonadState (get))
import Simulation (Simulation)
import System.Console.ANSI (clearScreen)

inspectMeanPrice :: Simulation ()
inspectMeanPrice = do
  (Account _ subscription@(Contract q1 c1 beta _ pm1) stake@(Asset _ c0 p0)) <- get
  liftIO
    $ do
      clearScreen >> putStr "\r"
      putStrLn $ "The mean price of your portfolio is " <> show pm1
      putStrLn $ "The starting price of your bitcoin was " <> show p0
      -- 100 - 400
      -- 0.1 btc <=> 400usd => 1Btc <=> 4000usd
      let p1 = (beta / (100 - beta)) * c1 / q1
      putStrLn $ "The final price to your bitcoin was " <> show p1
  return ()
