module Navigation where

type NavigationId = Int

data Navigation
  = Forward
      { fOption :: NavigationId
      }
  | Back