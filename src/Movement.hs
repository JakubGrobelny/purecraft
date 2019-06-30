module Movement (moveEntity) where

import Data.Map (toList)
import Control.Monad.State.Lazy
import Control.Monad
import Linear(V2(..))
import Foreign.C.Types


import World
import Entity
import Hitbox
import Physics
import Block
import Utility


numberOfSubsteps :: Int
numberOfSubsteps = 3

moveEntity :: World -> State Entity ()
moveEntity world = do
    entity <- get
    let speed        = physicsSpeed $ entityPhysics entity
        substepSpeed = (/ fromIntegral numberOfSubsteps) <$> speed
    case getSurroundings world entity of
        Nothing     -> return ()
        Just blocks -> do
            let blocksHB = HB $ map blockBoundingBox blocks
            replicateM_ numberOfSubsteps $ performSubstep substepSpeed blocksHB
            
stopEntity :: Entity -> Entity
stopEntity entity = entity { entityPhysics = static }
    where
        static = (entityPhysics entity) { physicsSpeed = V2 0.0 0.0 }

performSubstep :: V2 Double -> Hitbox -> State Entity ()
performSubstep speed blocksHB = do
    entity <- get
    let newHB = moveHB (entityHitbox entity) (floor <$> speed)
    if hitboxesCollide newHB blocksHB
        then
            put $ stopEntity entity
        else
            let pos  = entityPosition entity
                pos' = pos + (floor <$> speed) in
            put $ entity { entityPosition = pos', entityHitbox = newHB }

getSurroundings :: World -> Entity -> Maybe [Block]
getSurroundings world entity = do
    let pos         = entityPosition entity
        chunkId     = entityToChunkId entity
        speed       = physicsSpeed $ entityPhysics entity
        chunkNextId = chunkId + (if movingLeft entity then -1 else 1)
    chunk     <- lookupChunk world chunkId
    chunkNext <- lookupChunk world chunkNextId
    let blocks = getChunkSolidBlocks chunk chunkId ++ 
                 getChunkSolidBlocks chunkNext chunkNextId
    return blocks
