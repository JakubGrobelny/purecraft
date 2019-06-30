module Hitbox  where

import SDL
import Linear(V2(..))
import Foreign.C.Types
import Data.Function

import Block
import Utility


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
hitboxesCollide (HB hb0) (HB hb1) = or $ hb1 >>= \x -> map (bbsCollide x) hb0

blockBoundingBox :: Block -> BoundingBox
blockBoundingBox (Block (V2 x y) _) = BB x y blockSize blockSize

bbsCollide :: BoundingBox -> BoundingBox -> Bool
bbsCollide bb1 bb2 = not (x1' < x2 || x1 > x2' || y1' < y2 || y1 > y2')
    where
        x1 = bbX bb1
        y1 = bbY bb1
        x1' = x1 + bbWidth bb1
        y1' = y1 + bbHeight bb1
        x2 = bbX bb2
        y2 = bbY bb2
        x2' = x2 + bbWidth bb2
        y2' = y2 + bbHeight bb2

hbExtremePoint :: Hitbox
                  -> (CInt -> CInt -> Bool) 
                  -> (CInt -> CInt -> Bool) 
                  -> (CInt, CInt)
hbExtremePoint (HB bbs) cmpX cmpY = (extreme cmpX bbsXs, extreme cmpY bbsYs)
    where
        bbsXs :: [CInt]
        bbsXs = bbs >>= \bb -> [bbX bb, bbX bb + bbWidth bb]
        bbsYs :: [CInt]
        bbsYs = bbs >>= \bb -> [bbY bb, bbY bb + bbHeight bb]