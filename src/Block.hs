module Block where

import Foreign.C.Types
import Linear(V2(..))

import Hitbox


chunkHeight :: CInt
chunkHeight = 256

chunkWidth :: CInt
chunkWidth = 64

blockSize :: CInt
blockSize = 32

blockSizeV :: V2 CInt
blockSizeV = V2 blockSize blockSize

data Block
    = Air
    | Stone

blockHitbox :: (CInt, CInt) -> CInt -> Hitbox
blockHitbox (x, y) chunkId = HB [BB x' y blockSize blockSize]
    where
        x' = chunkId * chunkWidth + x

