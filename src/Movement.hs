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


moveEntity :: World -> State Entity ()
moveEntity world = do
    entity <- get
    let speed         = physicsSpeed $ entityPhysics entity
        blocks        = getSurroundings world entity
        blocksHB      = HB $ map blockBoundingBox blocks
        numOfSubsteps = divideIntoSubsteps speed
        substepSpeed  = (/ fromIntegral numOfSubsteps) <$> speed
    replicateM_ numOfSubsteps $ performSubstep substepSpeed blocksHB

divideIntoSubsteps :: V2 Double -> Int
divideIntoSubsteps (V2 x y) = damp 4 $ 1 + towardsInf (abs $ logBase 2.0 avg)
    where    
        avg = (x + y) / 2.0

stopEntity :: Axis -> State Entity ()
stopEntity axis = do
    entity <- get
    let speed = physicsSpeed $ entityPhysics entity
        staticX = (entityPhysics entity) { physicsSpeed = V2 0.0 (v2y speed) }
        staticY = (entityPhysics entity) { physicsSpeed = V2 (v2x speed) 0.0 }
    case axis of 
        XAxis -> put $ entity { entityPhysics = staticX }
        YAxis -> put $ entity 
                     { entityPhysics  = staticY
                     , entityGrounded = False
                     }

zeroSpeed :: V2 Double -> Axis -> V2 Double
zeroSpeed speed axis = case axis of
    XAxis -> V2 0.0 (v2y speed)
    YAxis -> V2 (v2x speed) 0.0

tryMoveOneDirection :: Axis -> V2 Double -> Hitbox -> State Entity ()
tryMoveOneDirection axis speed worldHB = do
    entity <- get
    let speed' = zeroSpeed speed $ otherAxis axis
        newHB  = moveHB (entityHitbox entity) $ towardsInf <$> speed'
    if hitboxesCollide newHB worldHB
        then stopEntity axis
        else do
            let pos  = entityPosition entity
                pos' =  pos + (towardsInf <$> speed')
            put $ entity
                { entityPosition = pos' 
                , entityHitbox   = newHB 
                }

performSubstep :: V2 Double -> Hitbox -> State Entity ()
performSubstep speed blocksHB = do
    entity <- get
    let newHB = moveHB (entityHitbox entity) (towardsInf <$> speed)
    if hitboxesCollide blocksHB newHB
        then do
            tryMoveOneDirection XAxis speed blocksHB
            tryMoveOneDirection YAxis speed blocksHB
        else
            let pos  = entityPosition entity
                pos' = pos + (towardsInf <$> speed)
            in put $ entity 
                { entityPosition = pos'
                , entityHitbox   = newHB
                }

canMoveOneDirection :: Hitbox -> Hitbox -> V2 Double -> Bool
canMoveOneDirection entityHB worldHB speed = not $ hitboxesCollide newHB worldHB
    where
        newHB = moveHB entityHB $ towardsInf <$> speed

getSurroundings :: World -> Entity -> [Block]
getSurroundings world entity =
    let pos       = entityPosition entity
        chunkId   = coordsToChunkId pos
        speed     = physicsSpeed $ entityPhysics entity
        fromPoint = startingPos speed $ entityHitbox entity
        toPoint   = endingPos speed $ entityHitbox entity
        fromX     = (min `on` v2x) fromPoint toPoint
        fromY     = (min `on` v2y) fromPoint toPoint
        toX       = (max `on` v2x) fromPoint toPoint
        toY       = (max `on` v2y) fromPoint toPoint
    in queryBlocks world (positions fromX fromY toX toY) isSolidBlock
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
            x <- [fromX - 1 .. toX + 1],
            y <- [fromY - 1 .. toY + 1]]
