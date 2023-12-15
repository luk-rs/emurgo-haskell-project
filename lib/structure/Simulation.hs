module Simulation where

import Account (Account)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Market (Market)

type Simulation a = ReaderT Market (StateT Account IO) a