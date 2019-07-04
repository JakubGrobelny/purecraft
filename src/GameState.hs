module GameState where

import qualified SDL as SDL
import qualified System.Random as R
import qualified Data.Map.Strict as Map
import           Data.Int
import           Linear (V2(..), V3(..))
import           Foreign.C.Types
import           Control.Monad.State.Lazy

import Controls
import Config
import Player
import World
import Entity
import Physics
import Movement
import Block
import Camera


data GameState = GameState
    { isExit         :: Bool
    , randoms        :: [Int]
    , gamePlayer     :: Player
    , gameCamera     :: Camera
    , gameController :: Controller
    , gameConfig     :: Configuration
    , gameWorld      :: World
    }

freshState :: Seed -> Configuration -> GameState
freshState seed cfg = GameState 
    { isExit         = False
    , randoms        = R.randoms (R.mkStdGen seed)
    , gamePlayer     = newPlayer
    , gameCamera     = cameraFromPlayer newPlayer $ fromIntegral <$> scrSize cfg
    , gameController = newController
    , gameConfig     = cfg
    , gameWorld      = newWorld seed
    }

wasWindowClosed :: [SDL.Event] -> Bool
wasWindowClosed = any (isWindowClose . SDL.eventPayload)
    where
        isWindowClose :: SDL.EventPayload -> Bool
        isWindowClose (SDL.WindowClosedEvent _ ) = True
        isWindowClose _ = False

updateState :: [SDL.Event] -> State GameState (GameState -> IO GameState)
updateState events = do
    state <- get
    let keys         = keyBindings $ gameConfig state
        (ctrl, events') = updateControls events keys $ gameController state
        world        = gameWorld state
        player       = gamePlayer state
        world'       = execState (ensureGenerated (entityPosition player)) world
        player'      = execState (moveEntity world') player
        camera       = gameCamera state
        mouseCoords  = mouseToCoords (playerAimPos ctrl) camera
    put $ state
        { gameController = ctrl
        , gamePlayer = acceleratePlayer ctrl player'
        , gameCamera = moveCamera camera player
        , gameWorld  = if playerLMB ctrl == StatePressed 
                       || playerLMB ctrl == StateActive
            then removeBlock world' ((`div` blockSize) <$> mouseCoords)
            else world'
        , isExit = wasWindowClosed events' || playerPauseActive ctrl
        }
    return $ \s -> do
        let V2 x y  = (`div` blockSize) <$> entityPosition player'
            chunkId = coordsToChunkId $ V2 x y
            speed   = (physicsSpeed . entityPhysics) player
        putStrLn $ "x: " ++ show x ++ 
                   " y: " ++ show y ++ 
                   " chunk: " ++ show chunkId ++
                   " speed: " ++ show speed
        putStrLn . show $ playerAimPos ctrl
        putStrLn . show $ entityHitbox player
        putStrLn . show $ entityGrounded player
        return s
        