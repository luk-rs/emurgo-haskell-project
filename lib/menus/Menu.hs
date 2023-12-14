module Menu where

import Entry (Entry)
import Navigation (NavigationId)

type MenuId = NavigationId

data Menu = Menu
  { mId :: MenuId
  , mLabel :: String
  , mEntries :: [Entry]
  }