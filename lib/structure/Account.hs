module Account where

import Asset (Asset (..))
import Book (Price)
import Contract (Contract (..))
import Market (Market)
import Ticker (Ticker)

type AccountId = Int
data Account = Account
  { acId :: AccountId
  , acContract :: Contract
  , acStake :: Asset
  }

emptyAccount :: Account
emptyAccount =
  Account
    { acId = 69
    , acContract = Unsubscribed
    , acStake = NotStaked
    }
