module World where

import qualified Data.Map as Map
import           Control.Monad.State.Lazy
import           Control.Monad
import           Foreign.C.Types
import           Linear(V2(..))
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Bits

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
            Just (Block (V2 x y) b) -> Just . flip Block b $ 
                V2 (x * blockSize + blockSize * chunkWidth * id) 
                   (y * blockSize)

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
    
removeBlock :: World -> V2 CInt -> World
removeBlock world coords = case lookupChunk world chunkId of
    Nothing -> world
    Just chunk -> let newBlocks = Map.insert coords' Air (chunkBlocks chunk)
                      chunk'    = chunk 
                                { chunkBlocks  = newBlocks
                                , chunkAltered = True 
                                }
                      chunks    = worldChunks world
        in world { worldChunks = Map.insert chunkId chunk' chunks }
    where
        chunkId = v2x coords `div` chunkWidth
        coords' = (v2x coords - chunkId * chunkWidth, v2y coords)

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
        [((x,y), Stone) | 
            x <- [0..chunkWidth-1], 
            y <- filter (isGround seed id x) [0..chunkHeight-1]]
    , chunkBackground = Map.empty
    , chunkAltered = False
    }
    where
        isGround :: Seed -> CInt -> CInt -> CInt -> Bool
        isGround seed chunkId x y = y > 115 + diff
            where
                s' = fromIntegral (seed `mod` 20)
                x' = 1 + abs x `mod` 20
                i' = abs chunkId `mod` x'
                diff = (s' + x' + i') `div` 6