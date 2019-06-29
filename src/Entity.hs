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

data Block
    = Air
    | Stone

updateEntity :: Entity -> Entity
updateEntity entity = entity
    { entityHitbox   = moveHB hb (pos' - pos)
    , entityPosition = pos'
    }
    where
        hb   = entityHitbox entity
        phs  = entityPhysics entity
        pos  = entityPosition entity
        pos' = applyPhysics phs pos
        moveHB :: Hitbox -> V2 CInt -> Hitbox
        moveHB hb amount = moveBB (fromIntegral <$> amount) <$> hb