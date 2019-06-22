{-# LANGUAGE RecordWildCards #-}

module GameState where

import SDL


data Effect = Effect

data GameState = GameState
    { stateIsExit  :: Bool }

freshState :: GameState
freshState = GameState {stateIsExit = False}

processEvents :: GameState -> [Event] -> (GameState, [Effect])
processEvents = undefined

gamePerformIO :: [Effect] -> GameState -> IO GameState
gamePerformIO [] state = return state
gamePerformIO (e:es) state = do
    newState <- performEffect e state
    gamePerformIO es newState

performEffect :: Effect -> GameState -> IO GameState
performEffect = undefined

drawState :: Renderer -> GameState -> IO ()
drawState renderer _ = do
    rendererDrawColor renderer $= V4 0 255 255 255
    clear renderer
    present renderer
