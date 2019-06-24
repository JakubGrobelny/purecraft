{-# LANGUAGE DeriveGeneric, MultiWayIf #-}

module Controls where

import qualified Data.Map.Strict as Map
import SDL
import GHC.Generics
import Data.Int
import Foreign.C.Types


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
            updateControls events keys $ updateControls' k m c
        _ -> let (c', events') = updateControls events keys c in 
            (c', ev : events')
    where
        updateControls' :: Keycode -> InputMotion -> Controller -> Controller
        updateControls' k m c
            | k == bindingUp    keys = c { movesUp    = m == Pressed }
            | k == bindingDown  keys = c { movesDown  = m == Pressed }
            | k == bindingLeft  keys = c { movesLeft  = m == Pressed }
            | k == bindingRight keys = c { movesRight = m == Pressed }
            | otherwise = c

movementToVector :: Controller -> V2 CInt
movementToVector c = V2 (xMov c) (yMov c)
    where
        intOfBool :: Bool -> CInt
        intOfBool True = 1
        intOfBool False = 0
        xMov :: Controller -> CInt
        xMov Controller{movesLeft = l, movesRight = r} =
            intOfBool r - intOfBool l
        yMov :: Controller -> CInt
        yMov Controller{movesUp = u, movesDown = d} =
            intOfBool d - intOfBool u

defaultKeyBindings :: KeyBindings
defaultKeyBindings = KeyBindings
    { bindingUp    = KeycodeW
    , bindingLeft  = KeycodeA
    , bindingRight = KeycodeD
    , bindingDown  = KeycodeS
    }