module Scene where

import Account (Account, emptyAccount)
import Data.Map (Map)
import Menu (Menu, MenuId, menusMap, startMenu)

data Scene = Scene
  { rMenus :: Map MenuId Menu
  , rMenu :: Menu
  , rAccount :: Account
  }

defaultRenderer :: Scene
defaultRenderer =
  Scene
    { rMenus = menusMap
    , rMenu = startMenu
    , rAccount = emptyAccount
    }