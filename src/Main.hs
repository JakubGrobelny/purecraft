{-# LANGUAGE OverloadedStrings #-}

module Main where

import SDL
import Linear (V4(..))
import Data.Aeson (decode)
import Data.String
import Control.Monad (unless)
import Foreign.C.Types
import System.Random

import Config
import GameState


main :: IO ()
main = do
    cfg <- readFile "res/config.json"
    case decode $ fromString cfg of
        Nothing -> putStrLn "Failed to load configuration file!"
        Just cfg -> do
            initializeAll
            window <- createWindow "Haskelike" $ customWindow cfg
            renderer <- createRenderer window (-1) defaultRenderer
            seed <- randomIO
            let state = freshState seed 
            gameLoop renderer state
            destroyWindow window
            quit

customWindow :: Configuration -> WindowConfig
customWindow cfg = defaultWindow 
    { windowInitialSize  = CInt . fromInteger <$> scrSize cfg
    , windowInputGrabbed = captureMouse cfg
    , windowPosition     = Centered 
    }

gameLoop :: Renderer -> GameState -> IO ()
gameLoop renderer state = do
    events <- pollEvents
    let (newState, effects) = updateState events state
    finalState <- gamePerformIO effects newState
    drawState renderer finalState
    unless (stateIsExit newState) (gameLoop renderer finalState)