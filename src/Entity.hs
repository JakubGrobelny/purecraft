module Entity where

import Foreign.C.Types
import Linear(V2(..))

import Physics
import Hitbox
import Utility


data Entity = Entity
    { entityPhysics  :: Physics
    , entityHitbox   :: Hitbox
    , entityPosition :: V2 CInt
    } deriving Show


accelerateEntity :: Entity -> Acceleration -> Entity
accelerateEntity entity accel = entity 
    { entityPhysics = applyAcceleration phs accel }
    where
        phs = entityPhysics entity

movingLeft :: Entity -> Bool
movingLeft = (0.0 <) . v2x . physicsSpeed . entityPhysics