module Player (acceleratePlayer, newPlayer, Player(..)) where

import Linear (V2(..))
import Foreign.C.Types
import Data.Int 
import Control.Monad.State.Lazy

import Controls
import Physics
import Entity
import Hitbox
import Utility
import Block


type Player = Entity

newPlayer :: Player
newPlayer = Entity 
    { entityPhysics  = Physics (V2 0 0) 0.75
    , entityHitbox   = HB [BB 0 (100 * blockSize) 32 80]
    , entityPosition = V2 0 (100 * blockSize)
    , entityGrounded = False
    }

verticalAcceleration :: Double
verticalAcceleration = 1.1

airborneVerticalHandicap :: Double
airborneVerticalHandicap = 0.4

jumpAcceleration :: Double
jumpAcceleration = -10.0

longJumpMult :: Double
longJumpMult = 0.1

sprintMult :: Double
sprintMult = 1.35

speedJumpBonus :: Double
speedJumpBonus = 0.02

acceleratePlayer :: Controller -> Player -> Player
acceleratePlayer ctrl player =
    let ground  = entityGrounded player
        xSpeed  = verticalAcceleration * controlsToXMovement ctrl
        xSpeed' = if ground then xSpeed else xSpeed * airborneVerticalHandicap
        ySpeed  = if playerMovesUp ctrl
            then if ground
                then jumpAcceleration
                else let speed = (v2y . physicsSpeed . entityPhysics) player in
                    if speed < 0.0
                        then speed * longJumpMult
                        else 0.0
            else 0.0
        vec = V2 xSpeed' $ ySpeed + (ySpeed * xSpeed' * speedJumpBonus)
        phs = entityPhysics player
    in player { entityPhysics = applyAcceleration phs ground vec }
    where
        controlsToXMovement :: Controller ->  Double
        controlsToXMovement ctrl = 
            (* if playerSprints  ctrl then sprintMult else 1.0) $ 
            (if playerMovesLeft  ctrl then -1.0 else 0.0) +
            (if playerMovesRight ctrl then  1.0 else 0.0)