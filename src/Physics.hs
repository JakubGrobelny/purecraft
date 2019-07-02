module Physics where

import Linear(V2(..))
import Foreign.C.Types

import Hitbox
import Utility


type Acceleration = V2 Double

gravity :: Double
gravity = 1.0

airDrag :: Double
airDrag = 0.045

airDragV :: V2 Double
airDragV = V2 (2 * airDrag) airDrag

groundDrag :: Double
groundDrag = 0.22

groundDragV :: V2 Double
groundDragV = V2 groundDrag 0.0

data Physics = Physics
    { physicsSpeed :: V2 Double
    , physicsMass  :: Double
    } deriving Show

applySpeed :: Physics -> V2 CInt -> V2 CInt
applySpeed phs pos = pos + speed
    where
        speed = round <$> physicsSpeed phs

applyAcceleration :: Physics -> Bool -> V2 Double -> Physics
applyAcceleration phs ground accel = phs { physicsSpeed = newSpeed }
    where
        oldSpeed     = physicsSpeed phs
        mass         = physicsMass phs
        gravityAccel = V2 0.0 $ if ground then 0.0 else mass * gravity
        xDrag        = if ground then groundDragV else v2Zero
        drag         = oldSpeed * (airDragV + xDrag)
        newSpeed     = oldSpeed + accel + gravityAccel - drag
