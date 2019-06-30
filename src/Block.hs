module Block where

import Foreign.C.Types
import Linear(V2(..))


data BlockType
    = Air
    | Stone
    deriving Show

data BackgroundType
    = AirBG
    deriving Show

data Block = Block (V2 CInt) BlockType deriving Show

data BGBlock = BGBlock (V2 CInt) BackgroundType deriving Show

chunkHeight :: CInt
chunkHeight = 256

chunkWidth :: CInt
chunkWidth = 64

blockSize :: CInt
blockSize = 32

blockSizeV :: V2 CInt
blockSizeV = V2 blockSize blockSize

isSolidBlockType :: BlockType -> Bool
isSolidBlockType Air = False
isSolidBlockType _ = True

isSolidBlock :: Block -> Bool
isSolidBlock (Block _ b) = isSolidBlockType b