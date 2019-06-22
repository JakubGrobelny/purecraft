{-# LANGUAGE OverloadedStrings #-}

module Main where

import SDL
import Linear (V4(..))
import Data.Aeson (decode)
import Data.String
import Control.Monad (unless)

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
            gameLoop renderer freshState

customWindow :: Configuration -> WindowConfig
customWindow cfg = defaultWindow 
    { windowInitialSize  = scrSize cfg
    , windowInputGrabbed = captureMouse cfg
    , windowPosition     = Centered }

gameLoop :: Renderer -> GameState -> IO ()
gameLoop renderer state = do
    events <- pollEvents
    let newState = processEvents state events
    drawState renderer newState
    unless (isExit newState) (gameLoop renderer newState)