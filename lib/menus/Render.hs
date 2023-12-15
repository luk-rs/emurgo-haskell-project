module Render where

import Account (Account, emptyAccount)
import Data.Map (Map)
import Menu (Menu, MenuId, menusMap, startMenu)

data Render = Render
  { rMenus :: Map MenuId Menu
  , rMenu :: Menu
  , rAccount :: Account
  }

defaultRenderer :: Render
defaultRenderer =
  Render
    { rMenus = menusMap
    , rMenu = startMenu
    , rAccount = emptyAccount
    }