{-# LANGUAGE RecordWildCards #-}

module GameState where

import SDL (V2(..), Event(..))
import qualified System.Random as R
import Foreign.C.Types
import qualified Data.Map.Strict as Map
import Control.Monad.State.Lazy

import Controls
import Config


data Effect = Effect

data Player = Player { playerPos :: V2 CInt }

data GameState = GameState
    { isExit         :: Bool
    , randoms        :: [Integer]
    , gamePlayer     :: Player
    , gameController :: Controller
    , gameConfig     :: Configuration
    }

freshState :: Int -> Configuration ->  GameState
freshState seed cfg = GameState 
    { isExit         = False
    , randoms        = R.randoms (R.mkStdGen seed)
    , gamePlayer     = Player $ V2 0 0
    , gameController = newController
    , gameConfig     = cfg
    }

updateState :: [Event] -> State GameState (IO ())
updateState events = do
    state <- get
    let keys         = keyBindings $ gameConfig state
        (c, events') = updateControls events keys $ gameController state
        player       = gamePlayer state
    put (state {gameController = c, gamePlayer = movePlayer player c})
    return $ return ()

movePlayer :: Player -> Controller -> Player
movePlayer (Player vec) = Player . (vec +) . movementToVector
