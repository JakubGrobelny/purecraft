module World where

import qualified Data.Map as Map
import           Control.Monad.State.Lazy
import           Control.Monad
import           Foreign.C.Types
import           Linear(V2(..))

import Entity
import Block


type Seed = Int

data Chunk = Chunk 
    { chunkBlocks  :: Map.Map (CInt, CInt) Block
    , chunkAltered :: Bool
    }

data World = World
    { worldSeed   :: Seed
    , worldChunks :: Map.Map CInt Chunk
    }

newWorld :: Seed -> World
newWorld seed = World
    { worldSeed   = seed
    , worldChunks = Map.empty
    }

coordsToChunkId :: V2 CInt -> CInt
coordsToChunkId (V2 x _) = x `div` (blockSize * chunkWidth)

simulationDist :: CInt
simulationDist = 8

ensureGenerated :: V2 CInt -> State World ()
ensureGenerated coords = do 
    let id = coordsToChunkId coords
    pruneWorld id
    forM_ [id - simulationDist .. id + simulationDist] ensureGeneratedId

pruneWorld :: CInt -> State World ()
pruneWorld id = do
    world <- get
    let chunks = worldChunks world
        pruned = Map.filterWithKey needed chunks
    put $ world { worldChunks = pruned }
    return ()
    where
        needed :: (CInt -> Chunk -> Bool)
        needed chunkId chunk = dist <= simulationDist || altered
            where
                dist = abs $ id - chunkId
                altered = chunkAltered chunk
    
ensureGeneratedId :: CInt -> State World ()
ensureGeneratedId id = do
    world <- get
    let chunks = worldChunks world
    case Map.lookup id chunks of
        Nothing -> do
            let chunk = generateChunk (worldSeed world) id
            put $ world { worldChunks = Map.insert id chunk chunks }
            return ()
        Just chunk -> return ()

lookupChunk :: World -> CInt -> Maybe Chunk
lookupChunk w = flip Map.lookup $ worldChunks w

-- TODO: add world generation
generateChunk :: Seed -> CInt -> Chunk
generateChunk seed id = Chunk
    { chunkBlocks = Map.fromList
        [((x,y), if y == 128 - id then Stone else Air) 
            | x <- [0..chunkWidth-1], y <- [0..chunkHeight-1]]
    , chunkAltered = False
    }