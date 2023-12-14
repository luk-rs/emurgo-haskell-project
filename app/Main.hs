module Main where

import Control.Monad.State (StateT (runStateT))
import GHC.IO.Handle (hSetBuffering, hSetEcho)
import System.IO (BufferMode (LineBuffering, NoBuffering), stdin, stdout)

import Menus (defaultRenderer, renderLoop)

setup :: IO ()
setup = do
  hSetBuffering stdout NoBuffering

flush :: IO ()
flush = do
  hSetBuffering stdout LineBuffering

main :: IO ()
main = do
  setup
  (_, _) <- runStateT renderLoop defaultRenderer
  flush

-- runStateT (flip runReaderT singleMarket $ runBook randomizeBookIO) emptyAccount