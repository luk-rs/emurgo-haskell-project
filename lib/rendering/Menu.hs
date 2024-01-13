module Menu where

import Data.Map (Map, fromList)
import Generics (pressAnyKey, toMap)

import Entry (Entry (..), Label)

-- import Inspection (inspectTrades)

import Control.Monad.RWS (MonadIO (liftIO))
import Inspection (inspectMeanPrice)
import Navigation (Navigation (..), NavigationId)
import Sifo (autoSifo)
import Simulation (Simulation)
import System.Console.ANSI (clearScreen)

type MenuId = NavigationId

data Menu = Menu
  { mId :: MenuId
  , mLabel :: String
  , mEntries :: [Entry]
  }

menusMap :: Map MenuId Menu
menusMap =
  let allMenus = [startMenu, simInspection]
   in fromList $ toMap allMenus mId

startMenu :: Menu
startMenu =
  Menu
    { mId = 1
    , mLabel = "Choose a simulation"
    , mEntries =
        [ newEntry 1 "Use a 80-20 split of portfolio strategy on s simulation" $ do
            autoSifo 80
            return (Forward 2)
        , newEntry 2 "Use a 60-40 split of portfolio strategy on s simulation" $ do
            autoSifo 60
            return (Forward 2)
        , newEntry 3 "Use a DCA investment strategy on a simulation" $ do
            liftIO $ do
              clearScreen >> putStr "\r"
              putStrLn "STILL UNDER CONSTRUCTION"
              pressAnyKey
            return (Forward 1)
        , newEntry 0 "Exit program" $ return Exit
        ]
    }

simInspection :: Menu
simInspection =
  Menu
    { mId = 2
    , mLabel = "Random Simulation"
    , mEntries =
        [ newEntry 1 "Mean Btc Pice" $ do
            inspectMeanPrice
            liftIO pressAnyKey
            return (Forward 2) -- inspectTrades
        , newEntry 2 "Final total valuations" $ do
            return (Forward 2)
        , newEntry 3 "Export partial valuations to csv"
            $ do
              liftIO $ do
                clearScreen >> putStr "\r"
                putStrLn "STILL UNDER CONSTRUCTION"
                pressAnyKey
              return (Forward 2)
        , newEntry 0 "Back to Main menu" $ return (Forward 1)
        ]
    }

newEntry :: MenuId -> Label -> Simulation Navigation -> Entry
newEntry id label sim = Entry{eId = id, eLabel = label, eSimulation = sim}
