module Render (drawState, GameRenderer(..), createGameRenderer) where 

import qualified Data.Map as Map
import           SDL
import           Foreign.C.Types
import           Linear(V2(..), V4(..))
import           GHC.Word

import GameState
import Player
import World
import Entity
import Block
import Hitbox

-- this renderer will hold the loaded textures
data GameRenderer = GameRenderer
    { sdlRenderer :: Renderer
    }

createGameRenderer :: Window -> IO GameRenderer
createGameRenderer window = do
    renderer <- createRenderer window (-1) defaultRenderer
    return $ GameRenderer renderer

drawPlayer :: GameRenderer -> Camera -> Player -> IO ()
drawPlayer r cam p = do
    let renderer = sdlRenderer r
        camPos   = cameraPos cam
        pPos     = entityPosition p
    rendererDrawColor renderer $= V4 0 63 127 255
    fillRect renderer $ Just $ Rectangle (P $ pPos - camPos) (V2 32 80)

drawBlock :: GameRenderer -> Camera -> Block -> IO ()
drawBlock r cam b@(Block pos block) = do
    let color    = blockColor block
        camPos   = cameraPos cam
        renderer = sdlRenderer r
    rendererDrawColor renderer $= color
    fillRect renderer $ Just $ Rectangle (P $ pos - camPos) blockSizeV
    
blockColor :: BlockType -> V4 Word8
blockColor Stone = V4 128 128 135 255
blockColor Air   = V4 0 0 0 0

drawWorld :: GameRenderer -> Camera -> World -> IO ()
drawWorld r cam world = mapM_ (drawBlock r cam) (fetchBlocks world)
    where
        fetchBlocks :: World -> [Block]
        fetchBlocks world = queryBlocks world coords isSolidBlock
        (V2 posX posY) = (`div` blockSize) <$> cameraPos cam
        (V2 resX resY) = (`div` blockSize) <$> cameraRes cam
        coords :: [(CInt, CInt)]
        coords = [(x, y) | 
            x <- [posX .. posX + resX],
            y <- [posY .. posY + resY]]

drawState :: GameRenderer -> GameState -> IO ()
drawState r state = do
    let renderer = sdlRenderer r
        cam      = gameCamera state
    rendererDrawColor renderer $= V4 125 170 230 255
    clear renderer
    drawWorld r cam $ gameWorld state
    drawPlayer r cam $ gamePlayer state
    present renderer
