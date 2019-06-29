module Hitbox where

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

type Hitbox = SingleOrList BoundingBox
    
instance Functor SingleOrList where
    fmap f (Single bb) = Single $ f bb
    fmap f (Multi bbs) = Multi $ map f bbs

moveBB :: V2 Int -> BoundingBox -> BoundingBox
moveBB (V2 deltaX deltaY) bb = bb
    { bbX = x + deltaX
    , bbY = y + deltaY
    }
    where
        x = bbX bb
        y = bbY bb

isColliding :: Hitbox -> Hitbox -> Bool
isColliding (Multi hbs1) (Multi hbs2) =
    any (\hb -> any ((isColliding $ Single hb) . Single) hbs2) hbs1 
isColliding (Multi hbs) (Single hb) = 
    any (\h -> isColliding (Single h) (Single hb)) hbs
isColliding (Single hb1) (Single hb2) = not $ x1' < x2 || x1 > x2' || 
                                              y1' < y2 || y1 > y2'
    where
        x1 = bbX hb1
        y1 = bbY hb1
        x1' = x1 + bbWidth hb1
        y1' = y1 + bbHeight hb1
        x2 = bbX hb2
        y2 = bbY hb2
        x2' = x1 + bbWidth hb2
        y2' = y1 + bbHeight hb2
isColliding hb hbs = isColliding hbs hb
