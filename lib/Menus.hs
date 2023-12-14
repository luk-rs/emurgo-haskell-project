module Menus where

import Control.Monad (forM_)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State (MonadIO (liftIO), MonadState (get), StateT (runStateT), forM)
import Data.Map (Map, fromList, lookup, (!))
import Sifo (Account, Simulation, emptyAccount, singleMarket)
import System.Exit (exitSuccess)
import Text.Read (readMaybe)
import Prelude hiding (lookup)

type MenuId = Int

data Menu = Menu
  { mId :: MenuId
  , mLabel :: String
  , mEntries :: [Entry]
  }

type EntryId = Int
data Entry = Entry
  { eId :: EntryId
  , eLabel :: String
  , eSimulation :: Simulation Navigation
  }

data Navigation
  = Forward
      { fOption :: MenuId
      }
  | Back

startMenu :: Menu
startMenu =
  Menu
    { mId = 1
    , mLabel = "Choose a simulation"
    , mEntries =
        [ Entry
            { eId = 1
            , eLabel = "Simulate manually inserting custom values"
            , eSimulation = return Back
            }
        , Entry
            { eId = 2
            , eLabel = "Simulate randomly on selected range for a given time"
            , eSimulation = return $ Forward 2
            }
        , Entry
            { eId = 0
            , eLabel = "Exit program"
            , eSimulation = return Back
            }
        ]
    }

manualSimMenu :: Menu
manualSimMenu =
  Menu
    { mId = 2
    , mLabel = ""
    , mEntries =
        [ Entry
            { eId = 0
            , eLabel = "Back to simulation selection menu"
            , eSimulation = return Back
            }
        ]
    }

flatten :: Menu -> (MenuId, Menu)
flatten menu = (mId menu, menu)

toMap :: [a] -> (a -> b) -> [(b, a)]
toMap [] _ = []
toMap (item : xs) f = (f item, item) : toMap xs f

entriesToMap :: [Entry] -> Map EntryId Entry
entriesToMap entries = fromList $ toMap entries eId

menusMap :: Map MenuId Menu
menusMap =
  let allMenus = [startMenu, manualSimMenu]
   in fromList $ toMap allMenus mId

type RenderKey = (MenuId, EntryId)
data Renderer = Renderer
  { rMenus :: Map MenuId Menu
  , rMenu :: Menu
  , rAccount :: Account
  }

defaultRenderer :: Renderer
defaultRenderer =
  Renderer
    { rMenus = menusMap
    , rMenu = startMenu
    , rAccount = emptyAccount
    }

type Render a = StateT Renderer IO a

renderLoop :: Render ()
renderLoop = do
  renderer <- get
  let menu = rMenu renderer
  renderMenu menu

-- TODO render Result and loop on the actual loop

renderMenu :: Menu -> Render ()
renderMenu menu = do
  let label = mLabel menu
      entries = mEntries menu
  putStrLn label
  printEntries entries
  putStr "input > "
  maybe <- readOption entries
  case maybe of
    Nothing -> do
      putStrLn "INVALID OPTION..PLEASE ENTER A VALID NUMBER"
      renderMenu menu
    Just entry -> do
      let sim = eSimulation entry
      (navigation, updatedAccount) <- runStateT (runReaderT sim singleMarket) emptyAccount -- TODO review this empty account
      -- probabaly we should use the renderer acocunt above ;)
      -- TODO choose what to do here with updated account
      -- modify the renderer account with the updated account
      case navigation of
        Forward id -> do
          renderMenu $ menusMap ! id
          renderMenu menu
        Back -> return ()

printEntries :: [Entry] -> IO ()
printEntries [] = return ()
printEntries (Entry{eId = id, eLabel = label} : xs) = do
  putStrLn $ "\t" ++ show id ++ " -> " ++ show label
  printEntries xs

readInt :: IO (Maybe Int)
readInt = readMaybe <$> getLine

readOption :: [Entry] -> IO (Maybe Entry)
readOption entries = do
  maybe <- readInt
  return $ case maybe of
    Nothing -> Nothing
    Just n -> lookup n $ entriesToMap entries