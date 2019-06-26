{-# LANGUAGE OverloadedStrings #-}

module Main where

import SDL
import Linear (V4(..))
import Data.Aeson (decode)
import Data.String
import Control.Monad (unless, when)
import Control.Monad.State.Lazy
import Control.Concurrent
import Foreign.C.Types
import System.Random

import Config
import GameState
import Render


main :: IO ()
main = do
    cfg <- readFile "res/config.json"
    case decode $ fromString cfg of
        Nothing -> putStrLn "Failed to load configuration file!"
        Just cfg -> do
            initializeAll
            window <- createWindow "PureCraft" $ customWindow cfg
            renderer <- createGameRenderer window
            seed <- randomIO
            let state = freshState seed cfg
            gameLoop renderer state
            destroyWindow window
            quit

customWindow :: Configuration -> WindowConfig
customWindow cfg = defaultWindow 
    { windowInitialSize  = CInt . fromInteger <$> scrSize cfg
    , windowInputGrabbed = captureMouse cfg
    , windowPosition     = Centered 
    }

gameLoop :: GameRenderer -> GameState -> IO ()
gameLoop renderer state = do
    t0 <- ticks
    events <- pollEvents
    let (effect, state') = runState (updateState events) state
    state'' <- effect state'
    drawState renderer state''
    t1 <- ticks
    let deltaT = t1 - t0
    when (deltaT <= 16) $ 
        threadDelay . fromIntegral . (* 1000) $ 16 - deltaT
    unless (isExit state'') $ 
        gameLoop renderer state''