module Renderer where

import Account (Account)
import Data.Map (Map)
import Menu (Menu, MenuId, menusMap, startMenu)
import Sifo (emptyAccount)

data Renderer = Renderer
  { rMenus :: Map MenuId Menu
  , rMenu :: Menu
  , rAccount :: Account
  }

defaultRenderer :: Renderer
defaultRenderer =
  Renderer
    { rMenus = menusMap
    , rMenu = startMenu
    , rAccount = emptyAccount
    }