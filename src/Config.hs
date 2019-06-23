{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Config where

import Data.Aeson
import Linear (V2(..))


data Configuration = Configuration
    { scrSize      :: V2 Integer
    , captureMouse :: Bool 
    }

instance FromJSON Configuration where
    parseJSON = withObject "config" $ \o -> do
        scrHeight    <- o .:? "height"       .!= 720
        scrWidth     <- o .:? "width"        .!= 1280
        captureMouse <- o .:? "captureMouse" .!= True
        let scrSize = V2 scrWidth scrHeight
        return Configuration{..}
