module Menu where

import Data.Map (Map, fromList)
import Generics (toMap)

import Account (Simulation)
import Entry (Entry (..), Label)
import Navigation (Navigation (..), NavigationId)

type MenuId = NavigationId

data Menu = Menu
  { mId :: MenuId
  , mLabel :: String
  , mEntries :: [Entry]
  }

menusMap :: Map MenuId Menu
menusMap =
  let allMenus = [startMenu, manualSimMenu]
   in fromList $ toMap allMenus mId

startMenu :: Menu
startMenu =
  Menu
    { mId = 1
    , mLabel = "Choose a simulation"
    , mEntries =
        [ newEntry 1 "Simulate manually inserting custom values" $ return Back
        , newEntry 2 "Simulate randomly on selected range for a given time" $ return (Forward 2)
        , newEntry 0 "Exit program" $ return Back
        ]
    }

manualSimMenu :: Menu
manualSimMenu =
  Menu
    { mId = 2
    , mLabel = ""
    , mEntries =
        [ newEntry 0 "Back to simulation selection menu" $ return Back
        ]
    }

newEntry :: MenuId -> Label -> Simulation Navigation -> Entry
newEntry id label sim = Entry{eId = id, eLabel = label, eSimulation = sim}
