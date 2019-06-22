module GameState where

import SDL

data GameState = GameState

freshState :: GameState
freshState = undefined

processEvents :: GameState -> [Event] -> GameState
processEvents = undefined

drawState :: Renderer -> GameState -> IO ()
drawState = undefined

isExit :: GameState -> Bool
isExit = undefined