module Main where

import Control.Monad.Reader (
  MonadIO (liftIO),
  MonadReader (ask),
  ReaderT (runReaderT),
 )
import Control.Monad.State (StateT (runStateT))
import GHC.IO.Handle (hSetEcho)
import Menus (defaultRenderer, renderLoop)
import Sifo (Book, Market (manualBookIO, randomizeBookIO), Simulation, emptyAccount, singleMarket)
import System.IO (stdin)

setup :: IO ()
setup = do
  hSetEcho stdin False

main :: IO ()
main = do
  runStateT renderLoop defaultRenderer
  -- runStateT (flip runReaderT singleMarket $ runBook randomizeBookIO) emptyAccount
  -- _ <- getChar
  putStrLn "gudbai"

-- runBook :: (Market -> IO Book) -> Simulation ()
-- runBook run = do
--   market <- ask
--   book <- liftIO $ run market
--   return ()