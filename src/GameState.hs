{-# LANGUAGE RecordWildCards #-}

module GameState where

import SDL
import qualified System.Random as R


data Effect = Effect

data GameState = GameState
    { stateIsExit  :: Bool
    , randoms      :: [Integer] }

freshState :: Int -> GameState
freshState seed = GameState { stateIsExit = False
                            , randoms     = R.randoms (R.mkStdGen seed)}

preprocessEvents :: [Event] -> [EventPayload]
preprocessEvents = map eventPayload

processEvents :: [EventPayload] -> GameState -> (GameState, [Effect])
processEvents (QuitEvent : _) st = (st {stateIsExit = True}, [])
processEvents (_:es) st = processEvents es st
processEvents [] st = (st, [])

gamePerformIO :: [Effect] -> GameState -> IO GameState
gamePerformIO [] state = return state
gamePerformIO (e:es) state = do
    newState <- performEffect e state
    gamePerformIO es newState

performEffect :: Effect -> GameState -> IO GameState
performEffect = undefined

drawState :: Renderer -> GameState -> IO ()
drawState renderer _ = do
    rendererDrawColor renderer $= V4 255 255 255 255
    clear renderer
    present renderer
