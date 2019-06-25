{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Config where

import Data.Aeson
import Linear (V2(..))
import SDL

import Controls


data Configuration = Configuration
    { scrSize      :: V2 Integer
    , captureMouse :: Bool
    , keyBindings  :: KeyBindings
    } deriving Show

instance FromJSON Configuration where
    parseJSON = withObject "config" $ \o -> do
        scrHeight    <- o .:? "height"       .!= 720
        scrWidth     <- o .:? "width"        .!= 1280
        captureMouse <- o .:? "captureMouse" .!= True
        keyBindings  <- o .:? "keyBindings"  .!= defaultKeyBindings
        let scrSize = V2 scrWidth scrHeight
        return Configuration{..}

instance FromJSON KeyBindings

instance ToJSON KeyBindings

instance FromJSON Keycode

instance ToJSON Keycode
