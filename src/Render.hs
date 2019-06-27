module Render where 

import qualified Data.Map as Map
import           SDL
import           Foreign.C.Types
import           Linear(V2(..), V4(..))
import           GHC.Word

import GameState
import Player
import World
import Entity


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
    rendererDrawColor renderer $= V4 255 0 0 255
    fillRect renderer $ Just $ Rectangle (P $ vec - camPos) (V2 100 100)

drawBlock :: GameRenderer -> Camera -> Block -> V2 CInt -> CInt -> IO ()
drawBlock r cam block pos chunkId = do
    let color    = blockColor block
        camPos   = cameraPos cam
        renderer = sdlRenderer r
        blockPos = offsetBlock (blockSizeV * pos) chunkId
    rendererDrawColor renderer $= color
    fillRect renderer $ Just $ Rectangle (P $ blockPos - camPos) blockSizeV
    where
        offsetBlock :: V2 CInt -> CInt -> V2 CInt
        offsetBlock (V2 x y) id = V2 (id * chunkWidth * blockSize + x) y

blockColor :: Block -> V4 Word8
blockColor Air = V4 155 175 255 255
blockColor Stone = V4 0 0 0 255

drawWorld :: GameRenderer -> Camera -> World -> IO ()
drawWorld r cam world = do
    let camPos = normalizedCameraPos cam
        chunkId = coordsToChunkId camPos
    drawChunk r cam world chunkId
    drawChunk r cam world $ chunkId - 1
    drawChunk r cam world $ chunkId + 1

drawChunk :: GameRenderer -> Camera -> World -> CInt -> IO ()
drawChunk r cam world id = case lookupChunk world id of
    Nothing -> return ()
    Just (Chunk blocks _) -> do
        let renderer = sdlRenderer r
        mapM_ (draw id) $ Map.toList blocks
    where
        draw :: CInt -> ((CInt, CInt), Block) -> IO ()
        draw chunkId ((x, y), b) = drawBlock r cam b (V2 x y) chunkId
        
drawState :: GameRenderer -> GameState -> IO ()
drawState renderer state = do
    let r   = sdlRenderer renderer
        cam = gameCamera state
    rendererDrawColor r $= V4 0 0 0 255
    clear r
    drawWorld renderer cam $ gameWorld state
    drawPlayer renderer cam $ gamePlayer state
    present r
