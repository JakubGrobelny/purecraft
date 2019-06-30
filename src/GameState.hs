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


data GameState = GameState
    { isExit         :: Bool
    , randoms        :: [Int]
    , gamePlayer     :: Player
    , gameCamera     :: Camera
    , gameController :: Controller
    , gameConfig     :: Configuration
    , gameWorld      :: World
    }

data Camera = Camera
    { cameraPos  :: V2 CInt
    , cameraZoom :: Double
    , cameraRes  :: V2 CInt
    }

cameraFromPlayer :: Player -> V2 CInt -> Camera
cameraFromPlayer p scr = Camera pos 1.0 scr
    where
        pos = (entityPosition p) - ((`div` 2) <$> scr)

freshState :: Seed -> Configuration ->  GameState
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
        (c, events') = updateControls events keys $ gameController state
        world        = gameWorld state
        player       = gamePlayer state
        world'       = execState (ensureGenerated (entityPosition player)) world
        (io, player')      = runState (moveEntity world') player
        camera       = gameCamera state
    put $ state
        { gameController = c
        , gamePlayer = acceleratePlayer player' c
        , gameCamera = moveCamera camera player
        , gameWorld  = world'
        , isExit = wasWindowClosed events' || pauseActive c
        }
    return $ \s -> do
        let V2 x y  = entityPosition player
            chunkId = coordsToChunkId $ V2 x y
            speed   = (physicsSpeed . entityPhysics) player
        putStrLn $ "x: " ++ show x ++ 
                   " y: " ++ show y ++ 
                   " chunk: " ++ show chunkId ++
                   " speed: " ++ show speed ++ (show $ entityHitbox player)
        io
        return s

moveCamera :: Camera -> Player -> Camera
moveCamera cam = flip cameraFromPlayer (cameraRes cam)

normalizedCameraPos :: Camera -> V2 CInt
normalizedCameraPos cam = cameraPos cam + ((`div` 2) <$> cameraRes cam)