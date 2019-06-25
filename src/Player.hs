module Player where

import Linear (V2(..))
import Foreign.C.Types

import Controls


data Player = Player 
    { playerPos :: V2 CInt 
    } deriving Show

movePlayer :: Player -> Controller -> Player
movePlayer (Player vec) = Player . (vec +) . (* 20) <$> movementToVector

