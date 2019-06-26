module World where

import qualified Data.Map as Map
import           Control.Monad.State.Lazy
import           Data.Int

import Entity


type Seed = Int

chunkHeight :: Int64
chunkHeight = 256

chunkWidth :: Int64
chunkWidth = 64

data Chunk = Chunk 
    { chunkBlocks  :: Map.Map (Int64, Int64) Block
    , chunkAltered :: Bool
    }

data World = World
    { worldSeed   :: Seed
    , worldChunks :: Map.Map Int64 Chunk
    }

newWorld :: Seed -> World
newWorld seed = World
    { worldSeed   = seed
    , worldChunks = Map.empty
    }

coordsToChunk :: (Int64, Int64) -> State World Chunk
coordsToChunk (x, y) = do
    world <- get
    let chunks  = worldChunks world
        chunkId = x `div` chunkWidth
    case Map.lookup chunkId chunks of
        Nothing    -> do
            let chunk = generateChunk (worldSeed world) chunkId
            put $ world { worldChunks = Map.insert chunkId chunk chunks }
            return chunk
        Just chunk -> return chunk

-- TODO: add world generation
generateChunk :: Seed -> Int64 -> Chunk
generateChunk _ _ = Chunk
    { chunkBlocks = Map.fromList
        [((x,y), if y <= 128 then Stone else Air ) 
            | x <- [0..chunkWidth-1], y <- [0..chunkHeight-1]]
    , chunkAltered = False
    }