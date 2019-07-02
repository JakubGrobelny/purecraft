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
    let speed    = physicsSpeed $ entityPhysics entity
        blocks   = getSurroundings world entity
        blocksHB = HB $ map blockBoundingBox blocks
        entityHB = entityHitbox entity
        pos      = entityPosition entity
        newSpeed = bisectMovement (v2Zero) speed entityHB blocksHB 10
    changeEntityPositionBy $ round <$> newSpeed
    tryMoveOneDirection YAxis speed blocksHB
    tryMoveOneDirection XAxis speed blocksHB
    checkIfGrounded blocksHB
    clearEntitySpeed

changeEntityPositionBy :: V2 CInt -> State Entity ()
changeEntityPositionBy amount = do
    entity <- get
    let pos = entityPosition entity
        hb  = entityHitbox entity
    put $ entity
        { entityPosition = pos + amount
        , entityHitbox = moveHB hb amount
        }

bisectMovement :: V2 Double -> V2 Double -> Hitbox -> Hitbox -> Int -> V2 Double
bisectMovement min max entityHB worldHB numOfSteps =
    if approxEq min max || numOfSteps == 0
        then min
        else if not $ hitboxesCollide (moveHB entityHB $ round <$> max) worldHB
            then max
            else let between = (/ 2.0) <$> (min + max)
                in if hitboxesCollide 
                        (moveHB entityHB $ round <$> between) worldHB
                    then bisectMovement 
                            min between entityHB worldHB (numOfSteps - 1)
                    else if isZero between || isBest between entityHB worldHB
                        then between
                        else bisectMovement 
                                between max entityHB worldHB (numOfSteps - 1)
    where
        approxEq :: V2 Double -> V2 Double -> Bool
        approxEq = (==) `on` (round <$>)
        normalize :: V2 Double -> V2 CInt
        normalize = (truncate . signum <$>)
        isBest :: V2 Double -> Hitbox -> Hitbox -> Bool 
        isBest mov entityHB worldHB =
            let mov' = (round <$> mov) + normalize mov
            in hitboxesCollide (moveHB entityHB mov') worldHB
        isZero :: V2 Double -> Bool
        isZero = and . ((== 0) <$>)

clearEntitySpeed :: State Entity ()
clearEntitySpeed = do
    entity <- get
    let phs   = entityPhysics entity
        speed = physicsSpeed phs
    put $ entity { entityPhysics = phs { physicsSpeed = fix <$> speed } }
    where
        fix :: Double -> Double
        fix x = if abs x < 0.3 then 0.0 else x

checkIfGrounded :: Hitbox -> State Entity ()
checkIfGrounded worldHB = do
    entity <- get
    let hbDown = moveHB (entityHitbox entity) $ V2 0 1
    if hitboxesCollide hbDown worldHB
        then do
            put $ entity { entityGrounded = True }
            stopEntity YAxis
        else put $ entity { entityGrounded = False }

stopEntity :: Axis -> State Entity ()
stopEntity axis = do
    entity <- get
    let speed = physicsSpeed $ entityPhysics entity
        staticX = (entityPhysics entity) { physicsSpeed = V2 0.0 (v2y speed) }
        staticY = (entityPhysics entity) { physicsSpeed = V2 (v2x speed) 0.0 }
    case axis of 
        XAxis -> put $ entity { entityPhysics = staticX }
        YAxis -> put $ entity { entityPhysics = staticY }

zeroSpeed :: V2 Double -> Axis -> V2 Double
zeroSpeed speed axis = case axis of
    XAxis -> V2 0.0 (v2y speed)
    YAxis -> V2 (v2x speed) 0.0

tryMoveOneDirection :: Axis -> V2 Double -> Hitbox -> State Entity ()
tryMoveOneDirection axis speed worldHB = do
    entity <- get
    let thisSpeed = v2Axis axis speed
        otherSpeed = v2Axis (otherAxis axis) speed
        mult   = abs . fixNaN $ thisSpeed / otherSpeed
        speed' = (* damp 1.0 mult) <$> (zeroSpeed speed $ otherAxis axis)
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