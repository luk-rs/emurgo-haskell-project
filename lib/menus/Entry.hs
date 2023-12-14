module Entry where

import Navigation (Navigation)
import Sifo (Simulation)

type EntryId = Int

data Entry = Entry
  { eId :: EntryId
  , eLabel :: String
  , eSimulation :: Simulation Navigation
  }
