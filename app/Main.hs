module Main where

import Control.Monad.State (StateT (runStateT))
import GHC.IO.Handle (hGetBuffering, hSetBuffering, hSetEcho)
import System.Console.ANSI (clearScreen)
import System.IO (BufferMode (LineBuffering, NoBuffering), stdin, stdout)

import Renderer (renderSimulator)

main :: IO ()
main = do
  restore <- setup
  renderSimulator
  flush restore

setup :: IO Restore
setup = do
  clearScreen
  stdinBuffer <- hGetBuffering stdin
  hSetBuffering stdin NoBuffering
  stdoutBuffer <- hGetBuffering stdout
  hSetBuffering stdout NoBuffering
  return
    Restore
      { rStdinBuffer = stdinBuffer
      , rStdoutBuffer = stdoutBuffer
      }

flush :: Restore -> IO ()
flush restore = do
  hSetBuffering stdout $ rStdoutBuffer restore
  hSetBuffering stdin $ rStdinBuffer restore
  clearScreen

data Restore = Restore
  { rStdinBuffer :: BufferMode
  , rStdoutBuffer :: BufferMode
  }