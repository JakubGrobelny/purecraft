module HitBox where

import SDL


data BoundingBox = BB
    { bbX :: Int
    , bbY :: Int
    , bbWidth  :: Int
    , bbHeight :: Int 
    } deriving Show

data HitBox
    = HB BoundingBox
    | MultiHB [BoundingBox]

collide :: HitBox -> HitBox -> Bool
collide (MultiHB hbs1) (MultiHB hbs2) =
    any (\hb -> any ((collide $ HB hb) . HB) hbs2) hbs1 
collide (MultiHB hbs) (HB hb) = any (\h -> collide (HB h) (HB hb)) hbs
collide (HB hb1) (HB hb2) = not $ x1' < x2 || x1 > x2' || y1' < y2 || y1 > y2'
    where
        x1 = bbX hb1
        y1 = bbY hb1
        x1' = x1 + bbWidth hb1
        y1' = y1 + bbHeight hb1
        x2 = bbX hb2
        y2 = bbY hb2
        x2' = x1 + bbWidth hb2
        y2' = y1 + bbHeight hb2
collide hb hbs = collide hbs hb
