module PhysicalBody where

import Linear (V2(..))
import Foreign.C.Types


data PhysicalBody 
    = PBodyRect
        { pbRectPos  :: V2 Double
        , pbRectSize :: V2 Double
        , pbRectProp :: PhysicalProp
        }
    | PBodyCirc
        { pbCircPos  :: V2 Integer
        , pbCircSize :: CInt
        , pbCircProp :: PhysicalProp
        }
    deriving Show

data PhysicalProp = PhysicalProp
    { mass :: Double
    , speed :: V2 Double
    } deriving Show