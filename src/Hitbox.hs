module Hitbox  where

import SDL
import Linear(V2(..))
import Foreign.C.Types

import Block


data BoundingBox = BB
    { bbX :: CInt
    , bbY :: CInt
    , bbWidth  :: CInt
    , bbHeight :: CInt 
    } deriving Show

newtype Hitbox = HB [BoundingBox] deriving Show

bbToRectangle :: BoundingBox -> V2 CInt -> Rectangle CInt
bbToRectangle bb offset = Rectangle (P $ V2 x y - offset) (V2 w h)
    where
        x = bbX bb
        y = bbY bb
        w = bbWidth bb
        h = bbHeight bb

moveBB :: V2 CInt -> BoundingBox -> BoundingBox
moveBB (V2 deltaX deltaY) bb = bb
    { bbX = x + deltaX
    , bbY = y + deltaY
    }
    where
        x = bbX bb
        y = bbY bb

moveHB :: Hitbox -> V2 CInt -> Hitbox
moveHB (HB bbs) amount = HB $ map (moveBB (fromIntegral <$> amount)) bbs

hitboxesCollide :: Hitbox -> Hitbox -> Bool
hitboxesCollide (HB hb0) (HB hb1) = or $ hb0 >>= \x -> map (bbsCollide x) hb1

blockBoundingBox :: Block -> BoundingBox
blockBoundingBox (Block (V2 x y) _) = BB x y blockSize blockSize

bbsCollide :: BoundingBox -> BoundingBox -> Bool
bbsCollide bb0 bb1 = not $ x1' < x2 || x1 > x2' || y1' > y2 || y1 < y2'
    where
        x1 = bbX bb0
        y1 = bbY bb0
        x1' = x1 + bbWidth bb0
        y1' = y1 + bbHeight bb0

        x2 = bbX bb1
        y2 = bbY bb1
        x2' = x1 + bbWidth bb1
        y2' = y1 + bbHeight bb1