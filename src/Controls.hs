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
    } deriving (Generic, Show)

defaultKeyBindings :: KeyBindings
defaultKeyBindings = KeyBindings
    { bindingUp    = KeycodeW
    , bindingLeft  = KeycodeA
    , bindingRight = KeycodeD
    , bindingDown  = KeycodeS
    , bindingPause = KeycodeEscape
    }

data Controller = Controller
    { aimPosX     :: Int32
    , aimPosY     :: Int32
    , movesLeft   :: Bool
    , movesRight  :: Bool
    , movesUp     :: Bool
    , movesDown   :: Bool
    , pauseActive :: Bool 
    } deriving Show

newController :: Controller
newController = Controller
    { aimPosX     = 0
    , aimPosY     = 0
    , movesLeft   = False
    , movesRight  = False
    , movesUp     = False
    , movesDown   = False
    , pauseActive = False
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
            | k == bindingUp    keys = c { movesUp     = m == Pressed }
            | k == bindingDown  keys = c { movesDown   = m == Pressed }
            | k == bindingLeft  keys = c { movesLeft   = m == Pressed }
            | k == bindingRight keys = c { movesRight  = m == Pressed }
            | k == bindingPause keys = c { pauseActive = m == Pressed }
            | otherwise = c

movementToVector :: Controller -> V2 Double
movementToVector c = V2 1.0 5.0 * V2 (xMov c) (yMov c)
    where
        doubleOfBool :: Bool -> Double
        doubleOfBool True = 1.0
        doubleOfBool False = 0.0
        xMov :: Controller -> Double
        xMov Controller{movesLeft = l, movesRight = r} =
            doubleOfBool r - doubleOfBool l
        yMov :: Controller -> Double
        yMov Controller{movesUp = u, movesDown = d} =
            doubleOfBool d - doubleOfBool u
