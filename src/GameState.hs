{-# LANGUAGE RecordWildCards #-}

module GameState where

import SDL
import qualified System.Random as R
import Foreign.C.Types
import qualified Data.Map.Strict as Map


data Effect = Effect

data Player = Player { playerPos :: V2 CInt }

data GameState = GameState
    { stateIsExit  :: Bool
    , randoms      :: [Integer]
    , player       :: Player
    , keyboard     :: Map.Map Keycode Bool
    }

freshState :: Int -> GameState
freshState seed = GameState { stateIsExit = False
                            , randoms     = R.randoms (R.mkStdGen seed)
                            , player      = Player $ V2 0 0
                            , keyboard    = Map.empty
                            }

updateState :: [Event] -> GameState -> (GameState, [Effect])
updateState events state = 
    if isExit
        then (state', [])
        else applyState state'
    where
        state' = processEvents (map eventPayload events) state
        isExit = stateIsExit state'

applyState :: GameState -> (GameState, [Effect])
applyState st = undefined

processEvents :: [EventPayload] -> GameState -> GameState
processEvents (QuitEvent : _) st = st {stateIsExit = True}
processEvents (KeyboardEvent e : es) st =
    case keysymKeycode $ keyboardEventKeysym e of
        KeycodeEscape -> st {stateIsExit = True}
        code -> processEvents es $ st {keyboard = Map.insert code active keys}
    where
        active = keyboardEventKeyMotion e == Pressed
        keys = keyboard st
processEvents (_:es) st = processEvents es st
processEvents [] st = st

gamePerformIO :: [Effect] -> GameState -> IO GameState
gamePerformIO [] state = return state
gamePerformIO (e:es) state = do
    newState <- performEffect e state
    gamePerformIO es newState

performEffect :: Effect -> GameState -> IO GameState
performEffect = undefined

drawState :: Renderer -> GameState -> IO ()
drawState renderer st = do
    rendererDrawColor renderer $= V4 255 255 255 255
    clear renderer
    rendererDrawColor renderer $= V4 0 0 0 255
    fillRect renderer $ Just (Rectangle (P . playerPos $ player st) 100)
    present renderer