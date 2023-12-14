module Sifo where

import Account (Account (..), Asset (..), Contract (..))

emptyAccount :: Account
emptyAccount =
  Account
    { acId = 231.231
    , acContract = Unsubscribed
    , acStake = NotStaked
    }