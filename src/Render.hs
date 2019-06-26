module Render where 

import SDL
import Foreign.C.Types
import Linear(V2(..))

import GameState
import Player
import World


-- this renderer will hold the loaded textures
data GameRenderer = GameRenderer
    { sdlRenderer :: Renderer
    }

createGameRenderer :: Window -> IO GameRenderer
createGameRenderer window = 
    GameRenderer <$> createRenderer window (-1) defaultRenderer

drawPlayer :: GameRenderer -> Camera -> Player -> IO ()
drawPlayer r cam (Player vec) = do
    let renderer = sdlRenderer r
        camPos   = cameraPos cam
        diff     = vec - camPos
    rendererDrawColor renderer $= V4 255 0 0 255
    fillRect renderer $ Just $ Rectangle (P $ vec - diff) (V2 100 100)

drawWorld :: GameRenderer -> Camera -> World -> IO ()
drawWorld _ _ _ = return ()

drawState :: GameRenderer -> GameState -> IO ()
drawState renderer state = do
    let r   = sdlRenderer renderer
        cam = gameCamera state
    rendererDrawColor r $= V4 255 255 255 255
    clear r
    drawWorld renderer cam $ gameWorld state
    drawPlayer renderer cam $ gamePlayer state
    present r
