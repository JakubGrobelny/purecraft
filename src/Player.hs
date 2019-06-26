module Player where

import Linear (V2(..))
import Foreign.C.Types

import Controls


data Player = Player 
    { playerPos :: V2 CInt 
    } deriving Show

newPlayer :: Player
newPlayer = Player $ V2 0 0

movePlayer :: Player -> Controller -> Player
movePlayer (Player vec) = Player . (vec +) . (* 20) <$> movementToVector
