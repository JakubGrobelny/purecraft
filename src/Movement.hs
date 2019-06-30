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
numberOfSubsteps = 4

moveEntity :: World -> State Entity ()
moveEntity world = do
    entity <- get
    let speed        = physicsSpeed $ entityPhysics entity
        substepSpeed = (/ fromIntegral numberOfSubsteps) <$> speed
    case getSurroundings world entity of
        Nothing     -> return ()
        Just blocks ->
            let blocksHB = HB $ map blockBoundingBox blocks in
            replicateM_ numberOfSubsteps $ performSubstep substepSpeed blocksHB

stopEntity :: Entity -> Entity
stopEntity entity = entity { entityPhysics = static }
    where
        static = (entityPhysics entity) { physicsSpeed = V2 0.0 0.0 }

performSubstep :: V2 Double -> Hitbox -> State Entity ()
performSubstep speed blocksHB = do
    entity <- get
    let newHB = moveHB (entityHitbox entity) (truncate <$> speed)
    if hitboxesCollide blocksHB newHB
        then
            put $ stopEntity entity
        else
            let pos  = entityPosition entity
                pos' = pos + (truncate <$> speed) in
            put $ entity { entityPosition = pos', entityHitbox = newHB }

getSurroundings :: World -> Entity -> Maybe [Block]
getSurroundings world entity = do
    let pos         = entityPosition entity
        chunkId     = coordsToChunkId pos
        speed       = physicsSpeed $ entityPhysics entity
    chunk  <- lookupChunk world chunkId
    chunkL <- lookupChunk world $ chunkId - 1
    chunkR <- lookupChunk world $ chunkId + 1
    let blocks = getChunkSolidBlocks chunk chunkId ++ 
                 getChunkSolidBlocks chunkL (chunkId - 1) ++
                 getChunkSolidBlocks chunkR (chunkId + 1)
    return blocks

