module Physics where

import Linear(V2(..))
import Foreign.C.Types


import Hitbox


gravity :: Double
gravity = 1.5

airDrag :: Double
airDrag = 0.03

airDragV :: V2 Double
airDragV = V2 airDrag airDrag

data Physics = Physics
    { physicsSpeed :: V2 Double
    , physicsMass  :: Double
    } deriving Show

applySpeed :: Physics -> V2 CInt -> V2 CInt
applySpeed phs pos = pos + speed
    where
        speed = round <$> physicsSpeed phs

applyAcceleration :: Physics -> V2 Double -> Physics
applyAcceleration phs accel = phs { physicsSpeed = newSpeed }
    where
        oldSpeed     = physicsSpeed phs
        mass         = physicsMass phs
        gravityAccel = V2 0.0 $ mass * gravity
        drag         = oldSpeed * airDragV
        newSpeed     = oldSpeed + accel + gravityAccel - drag
