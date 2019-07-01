{-# LANGUAGE DeriveGeneric #-}

module Controls where

import qualified Data.Map.Strict as Map
import           SDL
import           GHC.Generics
import           Data.Int
import           Foreign.C.Types


data KeyBindings = KeyBindings
    { bindingUp    :: Keycode
    , bindingDown  :: Keycode
    , bindingLeft  :: Keycode
    , bindingRight :: Keycode
    , bindingPause :: Keycode
    , bindingRun   :: Keycode
    } deriving (Generic, Show)

defaultKeyBindings :: KeyBindings
defaultKeyBindings = KeyBindings
    { bindingUp    = KeycodeW
    , bindingLeft  = KeycodeA
    , bindingRight = KeycodeD
    , bindingDown  = KeycodeS
    , bindingPause = KeycodeEscape
    , bindingRun   = KeycodeLShift
    }

data Controller = Controller
    { playerAimPosX     :: Int32
    , playerAimPosY     :: Int32
    , playerMovesLeft   :: Bool
    , playerMovesRight  :: Bool
    , playerMovesUp     :: Bool
    , playerPauseActive :: Bool
    , playerCrouches    :: Bool 
    , playerSprints     :: Bool
    } deriving Show

newController :: Controller
newController = Controller
    { playerAimPosX     = 0
    , playerAimPosY     = 0
    , playerMovesLeft   = False
    , playerMovesRight  = False
    , playerMovesUp     = False
    , playerPauseActive = False
    , playerCrouches    = False
    , playerSprints     = False
    }

updateControls :: [Event] -> KeyBindings -> Controller -> (Controller, [Event])
updateControls [] _ c = (c, [])
updateControls (ev : events) keys c =
    case eventPayload ev of
        KeyboardEvent (KeyboardEventData _ m _ (Keysym _ k _)) -> 
            updateControls events keys $ updateControls' k m c
        _ -> let (c', events') = updateControls events keys c in 
            (c', ev : events')
    where
        updateControls' :: Keycode -> InputMotion -> Controller -> Controller
        updateControls' k m c
            | k == bindingUp    keys = c { playerMovesUp     = m == Pressed }
            | k == bindingLeft  keys = c { playerMovesLeft   = m == Pressed }
            | k == bindingRight keys = c { playerMovesRight  = m == Pressed }
            | k == bindingDown  keys = c { playerCrouches    = m == Pressed }
            | k == bindingRun   keys = c { playerSprints     = m == Pressed }
            | k == bindingPause keys = c { playerPauseActive = m == Pressed }
            | otherwise = c