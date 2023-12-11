{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad
import System.IO

mainSetup :: IO ()
mainSetup = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering

main :: IO ()
main = do
  mainSetup
  onArrow $ \case
    UpKey -> putStrLn "es bem top"
    DownKey -> putStrLn "que depre"
    LeftKey -> putStrLn "es comuna caralho"
    RightKey -> putStrLn "heil mein meister"

data ArrowKey = UpKey | DownKey | LeftKey | RightKey deriving (Show)

onArrow :: (ArrowKey -> IO ()) -> IO ()
onArrow handler = do
  ch <- getChar
  res <- case ch of
    '\ESC' -> readEscaped [ch]
    _ -> print ch >> return Nothing
  forM_ res handler
  onArrow handler

readEscaped :: String -> IO (Maybe ArrowKey)
readEscaped s = do
  ch1 <- getChar
  ch2 <- getChar
  return $ findArrow (s ++ [ch1, ch2])

findArrow :: String -> Maybe ArrowKey
findArrow "\ESC[A" = Just UpKey
findArrow "\ESC[D" = Just LeftKey
findArrow "\ESC[B" = Just DownKey
findArrow "\ESC[C" = Just RightKey
findArrow _ = Nothing
