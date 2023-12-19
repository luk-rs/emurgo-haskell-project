module Generics where

import Control.Monad (unless)
import GHC.IO.Handle (BufferMode (..))
import System.Console.ANSI (clearScreen)
import System.IO (hGetBuffering, hSetBuffering, stdin, stdout)

type ErrorMessage = String

handlingError :: ErrorMessage -> IO ()
handlingError error = clearScreen >> unless (null error) (putStrLn error)

toMap :: [a] -> (a -> b) -> [(b, a)]
toMap [] _ = []
toMap (item : xs) f = (f item, item) : toMap xs f

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

pressAnyKey :: IO ()
pressAnyKey = putStrLn "Pres any key to continue.." >> getChar >> return ()