module Physics where

import Linear(V2(..))
import Foreign.C.Types


gravity :: Double
gravity = 1.0

airDrag :: Double
airDrag = 0.1

data Physics = Physics
    { physicsSpeed        :: V2 Double
    , physicsMass         :: Double
    } deriving Show

applyPhysics :: Physics -> V2 CInt -> V2 CInt
applyPhysics phs pos = pos + speed
    where
        speed = round <$> physicsSpeed phs
