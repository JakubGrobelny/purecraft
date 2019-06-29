module Hitbox  where

import SDL
import Linear(V2(..))
import Foreign.C.Types


data BoundingBox = BB
    { bbX :: Int
    , bbY :: Int
    , bbWidth  :: Int
    , bbHeight :: Int 
    } deriving Show

data SingleOrList a
    = Single a
    | Multi [a]
    deriving Show

newtype Hitbox = HB [BoundingBox] deriving Show

moveBB :: V2 Int -> BoundingBox -> BoundingBox
moveBB (V2 deltaX deltaY) bb = bb
    { bbX = x + deltaX
    , bbY = y + deltaY
    }
    where
        x = bbX bb
        y = bbY bb

hitboxesCollide :: Hitbox -> Hitbox -> Bool
hitboxesCollide (HB hb0) (HB hb1) = or $ hb0 >>= \x -> map (bbsCollide x) hb1

bbsCollide :: BoundingBox -> BoundingBox -> Bool
bbsCollide bb0 bb1 = not $ x1' < x2 || x1 > x2' || 
                           y1' < y2 || y1 > y2'
    where
    x1 = bbX bb0
    y1 = bbY bb0
    x1' = x1 + bbWidth bb0
    y1' = y1 + bbHeight bb0
    x2 = bbX bb1
    y2 = bbY bb1
    x2' = x1 + bbWidth bb1
    y2' = y1 + bbHeight bb1