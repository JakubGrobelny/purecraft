module Player where

import Linear (V2(..))
import Foreign.C.Types
import Data.Int 

import Controls
import Physics
import Entity
import Hitbox


type Player = Entity

newPlayer :: Player
newPlayer = Entity 
    { entityPhysics  = Physics (V2 0 0) 1.0
    , entityHitbox   = HB [BB 0 0 100 100]
    , entityPosition = V2 0 0 
    }

acceleratePlayer :: Player -> Controller -> Player
acceleratePlayer player ctrl = accelerateEntity player mov
    where 
        mov = movementToVector ctrl
