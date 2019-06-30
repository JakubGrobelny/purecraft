module Movement (moveEntity) where

import Data.Map (toList)
import Control.Monad.State.Lazy
import Control.Monad
import Linear(V2(..))
import Foreign.C.Types
import Data.Function
import Data.Maybe

import World
import Entity
import Hitbox
import Physics
import Block
import Utility


numberOfSubsteps :: Int
numberOfSubsteps = 4

moveEntity :: World -> State Entity (IO ())
moveEntity world = do
    entity <- get
    let speed        = physicsSpeed $ entityPhysics entity
        substepSpeed = (/ fromIntegral numberOfSubsteps) <$> speed
        (blocks, io) = getSurroundings world entity
        blocksHB = HB $ map blockBoundingBox blocks
    replicateM_ numberOfSubsteps $ performSubstep substepSpeed blocksHB
    return io

stopEntity :: Entity -> Entity
stopEntity entity = entity { entityPhysics = static }
    where
        static = (entityPhysics entity) { physicsSpeed = V2 0.0 0.0 }

performSubstep :: V2 Double -> Hitbox -> State Entity ()
performSubstep speed blocksHB = do
    entity <- get
    let newHB = moveHB (entityHitbox entity) (towardsInf <$> speed)
    if hitboxesCollide blocksHB newHB
        then
            put $ stopEntity entity
        else
            let pos  = entityPosition entity
                pos' = pos + (towardsInf <$> speed) in
            put $ entity { entityPosition = pos', entityHitbox = newHB }

getSurroundings :: World -> Entity -> ([Block], IO ())
getSurroundings world entity =
    let pos       = entityPosition entity
        chunkId   = coordsToChunkId pos
        speed     = (* 2.0) <$> physicsSpeed $ entityPhysics entity
        fromPoint = startingPos speed $ entityHitbox entity
        toPoint   = endingPos speed $ entityHitbox entity
        fromX     = (min `on` v2x) fromPoint toPoint
        fromY     = (min `on` v2y) fromPoint toPoint
        toX       = (max `on` v2x) fromPoint toPoint
        toY       = (max `on` v2y) fromPoint toPoint
        blocks    = queryBlocks world (positions fromX fromY toX toY) isSolidBlock
    in (blocks, putStrLn $ show blocks)
    where
        startingPos :: V2 Double -> Hitbox -> V2 CInt
        startingPos speed hb = (`div` blockSize) <$> (v2 $ 
            hbExtremePoint hb
                (if v2x speed > 0.0 then (<) else (>))
                (if v2y speed > 0.0 then (<) else (>)))
        endingPos :: V2 Double -> Hitbox -> V2 CInt
        endingPos speed hb = (`div` blockSize) <$> (v2 $
            hbExtremePoint (moveHB hb $ towardsInf <$> speed)
                (if v2x speed > 0.0 then (>) else (<))
                (if v2y speed > 0.0 then (>) else (<)))
        positions :: CInt -> CInt -> CInt -> CInt -> [(CInt, CInt)]
        positions fromX fromY toX toY = [(x, y) | 
            x <- [fromX .. toX ],
            y <- [fromY .. toY ]]