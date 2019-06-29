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
    , entityHitbox   = Multi []
    , entityPosition = V2 0 0 
    }

movePlayer :: Player -> Controller -> Player
movePlayer player ctrl = moved { entityPhysics = newPhs }
    where 
        pos = entityPosition player
        phs = entityPhysics player
        mov = movementToVector ctrl
        speed = round <$> physicsSpeed phs 
        newPhs = phs { physicsSpeed = fromIntegral <$> speed + mov }
        moved = updateEntity player