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

data ButtonState
    = StateActive
    | StatePressed
    | StateInactive
    deriving (Show, Eq, Ord)

motionToButtonState :: InputMotion -> ButtonState
motionToButtonState Pressed  = StatePressed
motionToButtonState Released = StateInactive

data Controller = Controller
    { playerAimPos      :: V2 Int32
    , playerMovesLeft   :: Bool
    , playerMovesRight  :: Bool
    , playerMovesUp     :: Bool
    , playerPauseActive :: Bool
    , playerCrouches    :: Bool 
    , playerSprints     :: Bool
    , playerLMB         :: ButtonState
    , playerRMB         :: ButtonState
    } deriving Show

newController :: Controller
newController = Controller
    { playerAimPos      = V2 0 0
    , playerMovesLeft   = False
    , playerMovesRight  = False
    , playerMovesUp     = False
    , playerPauseActive = False
    , playerCrouches    = False
    , playerSprints     = False
    , playerLMB         = StateInactive
    , playerRMB         = StateInactive
    }

updateControls :: [Event] -> KeyBindings -> Controller -> (Controller, [Event])
updateControls [] _ c = (c, [])
updateControls (ev : events) keys ctrl =
    case eventPayload ev of
        KeyboardEvent (KeyboardEventData _ m _ (Keysym _ k _)) -> 
            updateControls events keys $ updateKeys k m c
        MouseMotionEvent (MouseMotionEventData _ _ _ (P pos) _) ->
            updateControls events keys $ c { playerAimPos = pos }
        MouseButtonEvent mbEvent -> let button = mouseButtonEventButton mbEvent
                                        action = mouseButtonEventMotion mbEvent
            in updateControls events keys $ updateButtons button action c
        _ -> let (c', events') = updateControls events keys c in 
            (c', ev : events')
    where
        c = preprocessController ctrl
        
        preprocessController :: Controller -> Controller
        preprocessController ctrl = ctrl
            { playerLMB = if lmb == StatePressed then StateActive else lmb
            , playerRMB = if rmb == StatePressed then StateActive else rmb
            }
            where
                lmb = playerLMB ctrl
                rmb = playerRMB ctrl
        
        updateButtons :: MouseButton -> InputMotion -> Controller -> Controller
        updateButtons button motion ctrl =
            case button of
                ButtonLeft  -> ctrl { playerLMB = motionToButtonState motion }
                ButtonRight -> ctrl { playerRMB = motionToButtonState motion }
        
        updateKeys :: Keycode -> InputMotion -> Controller -> Controller
        updateKeys k m c
            | k == bindingUp    keys = c { playerMovesUp     = m == Pressed }
            | k == bindingLeft  keys = c { playerMovesLeft   = m == Pressed }
            | k == bindingRight keys = c { playerMovesRight  = m == Pressed }
            | k == bindingDown  keys = c { playerCrouches    = m == Pressed }
            | k == bindingRun   keys = c { playerSprints     = m == Pressed }
            | k == bindingPause keys = c { playerPauseActive = m == Pressed }
            | otherwise = c
