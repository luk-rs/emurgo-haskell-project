module Entry where

import Data.Map (Map, fromList, lookup)
import Generics (toMap)
import Text.Read (readMaybe)
import Prelude hiding (lookup)

import Navigation (Navigation)
import Simulation (Simulation)

type EntryId = Int
type Label = String

data Entry = Entry
  { eId :: EntryId
  , eLabel :: Label
  , eSimulation :: Simulation Navigation
  }

entriesToMap :: [Entry] -> Map EntryId Entry
entriesToMap entries = fromList $ toMap entries eId

readEntry :: [Entry] -> IO (Maybe Entry)
readEntry entries = do
  maybe <- readMaybe . flip (:) "" <$> getChar
  return $ case maybe of
    Nothing -> Nothing
    Just n -> lookup n $ entriesToMap entries

printEntries :: [Entry] -> IO ()
printEntries [] = return ()
printEntries (Entry{eId = id, eLabel = label} : xs) = do
  putStrLn $ "\t" ++ show id ++ " -> " ++ show label
  printEntries xs