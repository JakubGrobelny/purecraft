{-# LANGUAGE RecordWildCards #-}

module GameState where

import SDL
import qualified System.Random as R
import Foreign.C.Types
import qualified Data.Map.Strict as Map
import Control.Monad.State.Lazy

import Controls
import Config


data Effect = Effect

data Player = Player { playerPos :: V2 CInt }

data GameState = GameState
    { isExit       :: Bool
    , randoms      :: [Integer]
    , player       :: Player
    , controller   :: Controller
    , config       :: Configuration
    }

freshState :: Int -> Configuration ->  GameState
freshState seed cfg = GameState { isExit = False
                                , randoms     = R.randoms (R.mkStdGen seed)
                                , player      = Player $ V2 0 0
                                , controller  = newController
                                , config      = cfg
                                }

updateState :: [Event] -> GameState -> GameState
updateState _ st = st 

drawState :: Renderer -> GameState -> IO ()
drawState _ _ = return ()