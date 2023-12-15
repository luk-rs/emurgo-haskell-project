module Simulation where

import Account
import Control.Monad.Reader
import Control.Monad.State
import Market

type Simulation a = ReaderT Market (StateT Account IO) a