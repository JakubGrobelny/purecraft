module Utility where

import Linear(V2(..))


v2Max :: Ord a => V2 a -> a
v2Max (V2 a b)
    | a > b     = a
    | otherwise = b

v2x :: V2 a -> a
v2x (V2 x _ ) = x

v2y :: V2 a -> a
v2y (V2 _ y) = y    