module Main where

import Control.Monad.State (StateT (runStateT))
import GHC.IO.Handle (hGetBuffering, hSetBuffering, hSetEcho)
import System.Console.ANSI (clearScreen)
import System.IO (BufferMode (LineBuffering, NoBuffering), stdin, stdout)

import Generics (flush, setup)
import Renderer (renderSimulator)

main :: IO ()
main = do
  restore <- setup
  renderSimulator
  flush restore
