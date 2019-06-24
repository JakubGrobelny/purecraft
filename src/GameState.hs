{-# LANGUAGE RecordWildCards #-}

module GameState where

import Linear (V2(..))
import qualified SDL as SDL
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

wasWindowClosed :: [SDL.Event] -> Bool
wasWindowClosed = any (isWindowClose . SDL.eventPayload)
    where
        isWindowClose :: SDL.EventPayload -> Bool
        isWindowClose (SDL.WindowClosedEvent _ ) = True
        isWindowClose _ = False

updateState :: [SDL.Event] -> State GameState (IO ())
updateState events = do
    state <- get
    let keys         = keyBindings $ gameConfig state
        (c, events') = updateControls events keys $ gameController state
        player       = gamePlayer state
    put $ state 
        { gameController = c
        , gamePlayer = movePlayer player c
        , isExit = wasWindowClosed events' || pauseActive c
        }    
    return $ return ()

movePlayer :: Player -> Controller -> Player
movePlayer (Player vec) = Player . (vec +) . (* 20) <$> movementToVector
