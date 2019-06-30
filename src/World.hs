module World where

import qualified Data.Map as Map
import           Control.Monad.State.Lazy
import           Control.Monad
import           Foreign.C.Types
import           Linear(V2(..))
import           Data.Function
import           Data.List
import           Data.Maybe

import Entity
import Block
import Utility


type Seed = Int

data Chunk = Chunk 
    { chunkBlocks     :: Map.Map (CInt, CInt) BlockType
    , chunkBackground :: Map.Map (CInt, CInt) BackgroundType
    , chunkAltered :: Bool
    }

queryBlocks :: World -> [(CInt, CInt)] -> (Block -> Bool) -> [Block]
queryBlocks world coords which = filter which . catMaybes $ 
    splitCoords coords >>= (\(id, coords) -> getBlocks id coords)
    where
        splitCoords :: [(CInt, CInt)] -> [(CInt, [(CInt, CInt)])]
        splitCoords = map addId . groupBy ((==) `on` blockCoordsToChunkId)
            where
                addId :: [(CInt, CInt)] -> (CInt, [(CInt, CInt)])
                addId cs = ((blockCoordsToChunkId . head) cs, cs)
        fix :: CInt -> (CInt, CInt) -> (CInt, CInt)
        fix id (x, y) = (x - id * chunkWidth, y)
        getBlocks :: CInt -> [(CInt, CInt)] -> [Maybe Block]
        getBlocks id coords = case lookupChunk world id of
            Nothing -> []
            Just chunk -> map (fixBlock id . lookupBlock chunk . fix id) coords
        fixBlock :: CInt -> Maybe Block -> Maybe Block
        fixBlock id block = case block of
            Nothing -> Nothing
            Just (Block (V2 x y) b) -> Just $
                Block (V2 (x * blockSize + blockSize * chunkWidth * id) (y * blockSize)) b

lookupBlockV :: Chunk -> V2 CInt -> Maybe Block
lookupBlockV chunk (V2 x y) = lookupBlock chunk (x, y)

lookupBlock :: Chunk -> (CInt, CInt) -> Maybe Block
lookupBlock chunk coords = do
    bType <- Map.lookup coords (chunkBlocks chunk)
    return $ Block (v2 coords) bType

data World = World
    { worldSeed   :: Seed
    , worldChunks :: Map.Map CInt Chunk
    }

newWorld :: Seed -> World
newWorld seed = World
    { worldSeed   = seed
    , worldChunks = Map.empty
    }

entityToChunkId :: Entity -> CInt
entityToChunkId = coordsToChunkId . entityPosition 

blockCoordsToChunkId :: (CInt, CInt) -> CInt
blockCoordsToChunkId (x, y) = x `div` chunkWidth

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
        Just chunk -> return ()

lookupChunk :: World -> CInt -> Maybe Chunk
lookupChunk w = flip Map.lookup $ worldChunks w

-- TODO: add world generation
generateChunk :: Seed -> CInt -> Chunk
generateChunk seed id = Chunk
    { chunkBlocks = Map.fromList
        [((x,y), if y >= (if id `mod` 2 == 0 then (+) else (-)) 128 id then Stone else Air) | 
            x <- [0..chunkWidth-1], 
            y <- [0..chunkHeight-1]]
    , chunkBackground = Map.fromList 
        [((x, y), AirBG) | 
            x <- [0..chunkWidth-1],
            y <- [0..chunkHeight-1]
        ]
    , chunkAltered = False
    }