module Renderer where

import Control.Monad.Reader (runReaderT)
import Control.Monad.State (MonadIO (liftIO), MonadState (get), StateT (runStateT), modify)
import Control.Monad.State.Class (gets)
import Data.Map ((!))
import System.Console.ANSI (clearScreen)

import Account (Account, emptyAccount)
import Entry (Entry (eSimulation), printEntries, readEntry)
import Market (singleMarket)
import Menu (Menu (..), MenuId, menusMap, startMenu)
import Navigation (Navigation (..))
import Scene (Scene (..), defaultRenderer)

type Renderer a = StateT Scene IO a

renderSimulator :: IO ()
renderSimulator = do
  _ <- runStateT renderLoop defaultRenderer
  return ()

renderLoop :: Renderer ()
renderLoop = do
  renderer <- get
  let menu = rMenu renderer
      id = mId menu
  liftIO clearScreen
  navigation <- renderMenu menu
  case navigation of
    Back -> return ()
    Forward target -> do
      modify $ \renderer -> renderer{rMenu = menusMap ! target}
      renderLoop
      modify $ \renderer -> renderer{rMenu = menu}
      renderLoop

renderMenu :: Menu -> Renderer Navigation
renderMenu menu = do
  let label = mLabel menu
      entries = mEntries menu
  liftIO $ do
    putStrLn label
    printEntries entries
    putStr "input > "
  maybe <- liftIO $ readEntry entries
  case maybe of
    Nothing -> do
      liftIO $ clearScreen >> putStrLn "INVALID OPTION..PLEASE ENTER A VALID NUMBER"
      renderMenu menu
    Just entry -> do
      let sim = eSimulation entry
      (navigation, updatedAccount) <- do
        account <- gets rAccount
        liftIO $ runStateT (runReaderT sim singleMarket) account
      modify $ \renderer -> renderer{rAccount = updatedAccount}
      return navigation
