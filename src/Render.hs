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
    fillRect renderer $ Just $ Rectangle (P $ pPos - camPos) (V2 100 100)
    -- drawing the player hitbox for debugging purposes
    let HB bbs = entityHitbox p
    rendererDrawColor renderer $= V4 255 0 0 255
    mapM_ (drawRect renderer) $ map (Just . flip bbToRectangle camPos) bbs

drawBlock :: GameRenderer -> Camera -> Block -> IO ()
drawBlock r cam b@(Block pos block) = do
    let color    = blockColor block
        camPos   = cameraPos cam
        renderer = sdlRenderer r
    rendererDrawColor renderer $= color
    fillRect renderer $ Just $ Rectangle (P $ pos - camPos) blockSizeV
    -- drawing block's hitbox for debugging purposes
    if isSolidBlockType block
        then do 
            let bb = blockBoundingBox b
            rendererDrawColor renderer $= V4 255 0 0 255
            drawRect renderer $ Just $ bbToRectangle bb camPos
        else return ()
    
blockColor :: BlockType -> V4 Word8
blockColor Stone = V4 0 0 0 255
blockColor Air   = V4 155 175 255 255

drawWorld :: GameRenderer -> Camera -> World -> IO ()
drawWorld r cam world = mapM_ (drawBlock r cam) (fetchBlocks world)
    where
        fetchBlocks :: World -> [Block]
        fetchBlocks world = queryBlocks world coords (const True)
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
    rendererDrawColor renderer $= V4 0 0 0 255
    clear renderer
    drawWorld r cam $ gameWorld state
    drawPlayer r cam $ gamePlayer state
    present renderer
