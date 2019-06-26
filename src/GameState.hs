module GameState where

import qualified SDL as SDL
import qualified System.Random as R
import qualified Data.Map.Strict as Map
import           Data.Int
import           Linear (V2(..))
import           Foreign.C.Types
import           Control.Monad.State.Lazy

import Controls
import Config
import Player
import World


data GameState = GameState
    { isExit         :: Bool
    , randoms        :: [Int64]
    , gamePlayer     :: Player
    , gameController :: Controller
    , gameConfig     :: Configuration
    } deriving Show

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