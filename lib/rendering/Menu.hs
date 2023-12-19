module Menu where

import Data.Map (Map, fromList)
import Generics (toMap)

import Entry (Entry (..), Label)
import Inspection (inspectTrades)
import Navigation (Navigation (..), NavigationId)
import Sifo (autoSifo)
import Simulation (Simulation)

type MenuId = NavigationId

data Menu = Menu
  { mId :: MenuId
  , mLabel :: String
  , mEntries :: [Entry]
  }

menusMap :: Map MenuId Menu
menusMap =
  let allMenus = [startMenu, randomSimMenu, simInspection]
   in fromList $ toMap allMenus mId

startMenu :: Menu
startMenu =
  Menu
    { mId = 1
    , mLabel = "Choose a simulation"
    , mEntries =
        [ newEntry 1 "Simulate manually inserting custom values" $ return Back
        , newEntry 2 "Simulate randomly on selected range for a given time" $ return (Forward 2)
        , newEntry 0 "Exit program" $ return Exit
        ]
    }

randomSimMenu :: Menu
randomSimMenu =
  Menu
    { mId = 2
    , mLabel = "Random Simulation"
    , mEntries =
        [ newEntry 1 "Choose the number of weeks and price range and enjoy the beauty of mathematics" autoSifo
        , newEntry 0 "Back to simulation selection menu" $ return Back
        ]
    }

simInspection :: Menu
simInspection =
  Menu
    { mId = 3
    , mLabel = "Random Simulation"
    , mEntries =
        [ newEntry 1 "Inspect Trades" inspectTrades
        , newEntry 2 "Inspect Transaction details" $ return (Forward 1)
        , newEntry 3 "Inspect Rebalance outcome" $ return (Forward 1)
        , newEntry 0 "Back to Main menu" $ return (Forward 1)
        ]
    }

newEntry :: MenuId -> Label -> Simulation Navigation -> Entry
newEntry id label sim = Entry{eId = id, eLabel = label, eSimulation = sim}
