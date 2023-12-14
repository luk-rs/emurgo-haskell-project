module Generics where

toMap :: [a] -> (a -> b) -> [(b, a)]
toMap [] _ = []
toMap (item : xs) f = (f item, item) : toMap xs f