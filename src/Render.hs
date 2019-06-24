module Render where 

import SDL

import GameState


drawPlayer :: Renderer -> Player -> IO ()
drawPlayer renderer (Player (V2 x y)) = do
    rendererDrawColor renderer $= V4 255 0 0 255
    fillRect renderer $ Just $ Rectangle (P $ V2 x y) (V2 100 100)

drawState :: Renderer -> GameState -> IO ()
drawState renderer state = do
    rendererDrawColor renderer $= V4 255 255 255 255
    clear renderer
    drawPlayer renderer $ gamePlayer state
    present renderer
