module Menus where

import Control.Monad (forM_)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State (MonadIO (liftIO), MonadState (get), StateT (runStateT), forM, modify)
import Control.Monad.State.Class (gets)
import Data.Map (Map, fromList, lookup, (!))
import System.Exit (exitSuccess)
import Text.Read (readMaybe)
import Prelude hiding (lookup)

import Account (Account)
import Entry (Entry (..), EntryId, Label)
import Generics (toMap)
import Market (singleMarket)
import Menu (Menu (..), MenuId, menusMap, startMenu)
import Navigation (Navigation (Back, Forward))
import Renderer (Renderer (..))
import Sifo (emptyAccount)
import Simulation (Simulation)

type Render a = StateT Renderer IO a

renderLoop :: Render ()
renderLoop = do
  renderer <- get
  let menu = rMenu renderer
      id = mId menu
  navigation <- renderMenu menu
  case navigation of
    Back -> return ()
    Forward target -> do
      modify $ \renderer -> renderer{rMenu = menusMap ! target}
      renderLoop
      modify $ \renderer -> renderer{rMenu = menu}
      renderLoop

renderMenu :: Menu -> Render Navigation
renderMenu menu = do
  let label = mLabel menu
      entries = mEntries menu
  liftIO $ do
    putStrLn label
    printEntries entries
    putStr "input > "
  maybe <- liftIO $ readOption entries
  case maybe of
    Nothing -> do
      liftIO $ putStrLn "INVALID OPTION..PLEASE ENTER A VALID NUMBER"
      renderMenu menu
    Just entry -> do
      let sim = eSimulation entry
      (navigation, updatedAccount) <- do
        account <- gets rAccount
        liftIO $ runStateT (runReaderT sim singleMarket) account
      modify $ \renderer -> renderer{rAccount = updatedAccount}
      return navigation

printEntries :: [Entry] -> IO ()
printEntries [] = return ()
printEntries (Entry{eId = id, eLabel = label} : xs) = do
  putStrLn $ "\t" ++ show id ++ " -> " ++ show label
  printEntries xs

readOption :: [Entry] -> IO (Maybe Entry)
readOption entries = do
  maybe <- readInt
  return $ case maybe of
    Nothing -> Nothing
    Just n -> lookup n $ entriesToMap entries

readInt :: IO (Maybe Int)
readInt = readMaybe <$> getLine

entriesToMap :: [Entry] -> Map EntryId Entry
entriesToMap entries = fromList $ toMap entries eId