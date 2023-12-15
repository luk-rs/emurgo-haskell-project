module Entry where

import Data.Map (Map, fromList)

import Account (Simulation)
import Navigation (Navigation)

type EntryId = Int
type Label = String

data Entry = Entry
  { eId :: EntryId
  , eLabel :: Label
  , eSimulation :: Simulation Navigation
  }