{-# LANGUAGE DeriveGeneric, MultiWayIf #-}

module Controls where

import qualified Data.Map.Strict as Map
import SDL
import GHC.Generics
import Data.Int


data KeyBindings = KeyBindings
    { bindingUp    :: Keycode
    , bindingDown  :: Keycode
    , bindingLeft  :: Keycode
    , bindingRight :: Keycode
    } deriving (Generic, Show)

data Controller = Controller
    { aimPosX    :: Int32
    , aimPosY    :: Int32
    , movesLeft  :: Bool
    , movesRight :: Bool
    , movesUp    :: Bool
    , movesDown  :: Bool
    }

newController :: Controller
newController = Controller
    { aimPosX    = 0
    , aimPosY    = 0
    , movesLeft  = False
    , movesRight = False
    , movesUp    = False
    , movesDown  = False
    }

updateControls :: [Event] -> KeyBindings -> Controller -> (Controller, [Event])
updateControls [] _ c = (c, [])
updateControls (ev : events) keys c =
    case eventPayload ev of
        KeyboardEvent (KeyboardEventData _ m _ (Keysym _ k _)) -> 
            let c' = updateControls' k m c in updateControls events keys c'
        _ -> let (c', events') = updateControls events keys c in 
            (c', ev : events')
    where
        updateControls' :: Keycode -> InputMotion -> Controller -> Controller
        updateControls' k m c
            | k == bindingUp keys    = c { movesUp    = m == Pressed }
            | k == bindingDown keys  = c { movesDown  = m == Pressed }
            | k == bindingLeft keys  = c { movesLeft  = m == Pressed }
            | k == bindingRight keys = c { movesRight = m == Pressed }

defaultKeyBindings :: KeyBindings
defaultKeyBindings = KeyBindings
    { bindingUp    = KeycodeUp
    , bindingLeft  = KeycodeLeft
    , bindingRight = KeycodeRight
    , bindingDown  = KeycodeDown
    }
