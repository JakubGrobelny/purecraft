module Physics where

import Linear(V2(..))
import Foreign.C.Types

import Hitbox


type Acceleration = V2 Double

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

applyAcceleration :: Physics -> Bool -> V2 Double -> Physics
applyAcceleration phs ground accel = phs { physicsSpeed = normalize newSpeed }
    where
        oldSpeed     = physicsSpeed phs
        mass         = physicsMass phs
        gravityAccel = V2 0.0 $ if ground then 0.0 else mass * gravity
        drag         = oldSpeed * airDragV
        newSpeed     = oldSpeed + accel + gravityAccel - drag
        normalize :: V2 Double -> V2 Double
        normalize (V2 x y) = V2
            (if (abs x) < 1.0 then 0.0 else x)
            (if (abs y) < 1.0 then 0.0 else y)