module Entry where

import Data.Map (Map, fromList)
import Navigation (Navigation)
import Simulation (Simulation)

type EntryId = Int
type Label = String

data Entry = Entry
  { eId :: EntryId
  , eLabel :: Label
  , eSimulation :: Simulation Navigation
  }