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
    { entityPhysics  = Physics (V2 0 0) 1.0
    , entityHitbox   = HB [BB 0 (100 * blockSize) 48 80]
    , entityPosition = V2 0 (100 * blockSize)
    , entityGrounded = False
    }

verticalAcceleration :: Double
verticalAcceleration = 3.0

airborneVerticalHandicap :: Double
airborneVerticalHandicap = 0.25

jumpAcceleration :: Double
jumpAcceleration = -25.0

acceleratePlayer :: Controller -> State Player ()
acceleratePlayer ctrl = do
    player <- get
    let ground  = entityGrounded player
        xSpeed  = verticalAcceleration * controlsToXMovement ctrl
        xSpeed' = if ground then xSpeed else xSpeed * airborneVerticalHandicap
        ySpeed  = if ground && playerMovesUp ctrl
            then jumpAcceleration
            else 0.0
        vec = V2 xSpeed' ySpeed
        phs = entityPhysics player
    put $ player { entityPhysics = applyAcceleration phs ground vec }
    where
        controlsToXMovement :: Controller ->  Double
        controlsToXMovement ctrl = 
            (if playerMovesLeft  ctrl then -1.0 else 0.0) +
            (if playerMovesRight ctrl then  1.0 else 0.0)
