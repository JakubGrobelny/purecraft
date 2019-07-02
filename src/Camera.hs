module Camera where

import Foreign.C.Types
import Linear(V2(..))
import Data.Int

import Player
import Entity


data Camera = Camera
    { cameraPos  :: V2 CInt
    , cameraZoom :: Double
    , cameraRes  :: V2 CInt
    }

cameraFromPlayer :: Player -> V2 CInt -> Camera
cameraFromPlayer p scr = Camera pos 1.0 scr
    where
        pos = (entityPosition p) - ((`div` 2) <$> scr)

moveCamera :: Camera -> Player -> Camera
moveCamera cam = flip cameraFromPlayer (cameraRes cam)

normalizedCameraPos :: Camera -> V2 CInt
normalizedCameraPos cam = cameraPos cam + ((`div` 2) <$> cameraRes cam)

mouseToCoords :: V2 Int32 -> Camera -> V2 CInt
mouseToCoords pos cam = pos' + cPos
    where
        cPos = cameraPos cam
        pos' = fromIntegral <$> pos
