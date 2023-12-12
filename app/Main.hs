module Main where

import Control.Monad.Reader (
  MonadIO (liftIO),
  MonadReader (ask),
  ReaderT (runReaderT),
 )
import Control.Monad.State (StateT (runStateT))
import Sifo (
  Market (manualBookIO, randomizeBookIO),
  Simulation,
  emptyAccount,
  singleMarket,
 )

main :: IO ()
main = do
  runStateT (runReaderT randomizeBook singleMarket) emptyAccount
  _ <- getChar
  putStrLn "gudbai"

randomizeBook :: Simulation ()
randomizeBook = do
  market <- ask
  book <- liftIO $ manualBookIO market
  return ()