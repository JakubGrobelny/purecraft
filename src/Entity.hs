module Entity where

import Foreign.C.Types
import Linear(V2(..))

import Physics
import Hitbox


data Entity = Entity
    { entityPhysics  :: Physics
    , entityHitbox   :: Hitbox
    , entityPosition :: V2 CInt
    } deriving Show

updateEntity :: Entity -> V2 Double -> Entity
updateEntity entity accel = entity
    { entityHitbox   = moveHB hb (pos' - pos)
    , entityPosition = pos'
    , entityPhysics  = applyAcceleration phs accel
    }
    where
        hb   = entityHitbox entity
        phs  = entityPhysics entity
        pos  = entityPosition entity
        pos' = applySpeed phs pos
        moveHB :: Hitbox -> V2 CInt -> Hitbox
        moveHB (HB hb) amount = HB $ moveBB (fromIntegral <$> amount) <$> hb