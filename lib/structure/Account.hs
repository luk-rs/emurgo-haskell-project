module Account where

import Asset (Asset (..))
import Book (Price)
import Contract (Contract (..))
import Market (Market)
import Ticker (Ticker)

type AccountId = Double
data Account = Account
  { acId :: AccountId
  , acContract :: Contract
  , acStake :: Asset
  }

emptyAccount :: Account
emptyAccount =
  Account
    { acId = 231.231
    , acContract = Unsubscribed
    , acStake = NotStaked
    }
