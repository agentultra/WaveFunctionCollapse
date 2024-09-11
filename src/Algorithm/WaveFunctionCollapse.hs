{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Algorithm.WaveFunctionCollapse where

import qualified Debug.Trace as Debug

import Control.Monad
import Control.Monad.State.Strict
import Data.Array (Array, (!))
import qualified Data.Array as Array
import Data.Bifunctor
import Data.Heap (MinHeap)
import qualified Data.Heap as Heap
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
-- TODO: REMOVE ME
import Data.Set (Set)
import qualified Data.Set as Set
-- TODO: /REMOVE ME
import Data.Foldable
import Data.Maybe
import System.Random
import Test.QuickCheck

data Pattern a
  = Pattern
  { patternSize :: Word
  , getPattern  :: Array (Word, Word) a
  }
  deriving (Eq, Ord, Show)

instance Arbitrary a => Arbitrary (Pattern a) where
  arbitrary = do
    s <- chooseInt (1, 10)
    xs <- infiniteList
    pure . Pattern (toEnum s) $ Array.listArray ((0, 0), (toEnum s - 1, toEnum s - 1)) $ take (s * s) xs

mkFillPattern :: a -> Word -> Maybe (Pattern a)
mkFillPattern fillValue size
  | size == 0 = Nothing
  | otherwise
  = Just
  . Pattern size
  . Array.listArray ((0,0), (size, size))
  $ repeat fillValue

patternValue :: Pattern a -> Word -> Word -> Maybe a
patternValue p x y
  | x < patternSize p && y < patternSize p = Just $ getPattern p ! (x, y)
  | otherwise = Nothing

newtype Texture a = Texture { getTexture :: Array (Word, Word) a }
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Texture a) where
  arbitrary = do
    s <- chooseInt (2, 20)
    xs <- infiniteList
    pure . Texture $ Array.listArray ((0, 0), (toEnum s - 1, toEnum s - 1)) $ take (s * s) xs

mkTexture :: a -> Word -> Texture a
mkTexture fillValue size
  = Texture
  . Array.listArray ((0,0), (size - 1, size - 1))
  $ repeat fillValue

textureFromList :: Word -> [a] -> Texture a
textureFromList size xs
  = Texture
  . Array.listArray ((0, 0), (size - 1, size - 1))
  $ xs

textureSize :: Texture a -> Word
textureSize
  = toEnum
  . (+1)
  . fromIntegral
  . fst
  . snd
  . Array.bounds
  . getTexture

patterns :: Texture a -> Word -> PatternResult a
patterns texture subPatternSize
  | subPatternSize == 0 = PatternResult [] 0
  | otherwise =
    let upTo = textureSize texture - 1
        ps = [ extractPattern texture (x, y) subPatternSize
             | x :: Word <- [0..upTo], y :: Word <- [0..upTo]
             ]
    in PatternResult ps (length ps - 1)
  where
    extractPattern :: Texture a -> (Word, Word) -> Word -> Pattern a
    extractPattern (Texture tex) (x, y) size =
      let w = size - 1
          indices = [ (a `mod` w, b `mod` w) | a <- [x..x+w], b <- [y..y+w] ]
          textureElems = foldl' (accumElems tex) [] indices
      in Pattern size $ Array.listArray ((0, 0), (size - 1, size - 1)) textureElems

    accumElems :: Array (Word, Word) a -> [a] -> (Word, Word) -> [a]
    accumElems tex acc ix = (tex Array.! ix) : acc

data Direction
  = Up
  | Right'
  | Down
  | Left'
  deriving (Bounded, Enum, Eq, Ord, Show)

instance Arbitrary Direction where
  arbitrary = oneof [pure Up, pure Right', pure Down, pure Left']

directions :: [Direction]
directions = [Up, Right', Down, Left']

cellIxForDirection :: (Int, Int) -> (Int, Int) -> Direction -> (Int, Int)
cellIxForDirection (_, maxY) (cellX, cellY) Up
  = (cellX, cellY - 1 `mod` maxY)
cellIxForDirection (maxX, _) (cellX, cellY) Right'
  = (cellX + 1 `mod` maxX, cellY)
cellIxForDirection (_, maxY) (cellX, cellY) Down
  = (cellX, cellY + 1 `mod` maxY)
cellIxForDirection (maxX, _) (cellX, cellY) Left'
  = (cellX - 1 `mod` maxX, cellY)

-- | Check if two patterns overlap, offset in a given Direction by 1.
overlaps :: Eq a => Pattern a -> Pattern a -> Direction -> Bool
overlaps p1 p2 = \case
  Up ->
    let p1Values = catMaybes [ patternValue p1 x y | x <- [0..patternSize p1 - 1], y <- [0..patternSize p1 - 2] ]
        p2Values = catMaybes [ patternValue p2 x y | x <- [0..patternSize p1 - 1], y <- [1..patternSize p1 - 1] ]
    in all elemEq $ zip p1Values p2Values
  Right' ->
    let p1Values = catMaybes [ patternValue p1 x y | x <- [0..patternSize p1 - 2], y <- [0..patternSize p1 - 1] ]
        p2Values = catMaybes [ patternValue p2 x y | x <- [0..patternSize p2 - 2], y <- [0..patternSize p2 - 1] ]
    in all elemEq $ zip p1Values p2Values
  Down ->
    let p1Values = catMaybes [ patternValue p1 x y | x <- [0..patternSize p1 - 1], y <- [1..patternSize p1 - 1] ]
        p2Values = catMaybes [ patternValue p2 x y | x <- [0..patternSize p2 - 1], y <- [0..patternSize p2 - 2] ]
    in all elemEq $ zip p1Values p2Values
  Left' ->
    let p1Values = catMaybes [ patternValue p1 x y | x <- [1..patternSize p1 - 2], y <- [0..patternSize p1 - 1] ]
        p2Values = catMaybes [ patternValue p2 x y | x <- [1..patternSize p2 - 1], y <- [0..patternSize p2 - 1] ]
    in all elemEq $ zip p1Values p2Values
  where
    elemEq (x, y) = x == y

clockwise :: Pattern a -> Pattern a
clockwise pat
  = Pattern pat.patternSize
  . Array.listArray (Array.bounds pat.getPattern)
  . concat
  . fmap reverse
  . fmap (fmap snd)
  . List.groupBy (\x y -> (fst . fst $ x) == (fst . fst $ y))
  . Array.assocs
  $ pat.getPattern

newtype FrequencyHints = FrequencyHints { getFrequencyHints :: Map PatternIndex Int }
  deriving (Eq, Show)

relativeFrequency :: PatternIndex -> FrequencyHints -> Int
relativeFrequency ix hints =
  let hintMap = getFrequencyHints hints
      ixFrequencyHint = maybe 0 id . Map.lookup ix $ hintMap
  in floor $ fromIntegral ixFrequencyHint * log (fromIntegral ixFrequencyHint)

frequencyHints :: Ord a => [Pattern a] -> FrequencyHints
frequencyHints ps = FrequencyHints . go Map.empty ps . zip [0..] $ ps
  where
    go :: Ord a => Map Int Int -> [Pattern a] -> [(Int, Pattern a)] -> Map Int Int
    go accum _ [] = accum
    go accum ps' ((ix,p):xs) = go (Map.insert ix (seen p ps') accum) ps' xs

    seen :: Eq a => Pattern a -> [Pattern a] -> Int
    seen x = List.foldl' (\count y -> if x == y then (count + 1) else count) 0

data PatternResult a
  = PatternResult
  { patternResultPatterns :: [Pattern a]
  , patternResultMaxIndex :: PatternIndex
  }
  deriving (Eq, Show)

type PatternIndex = Int

data AdjacencyKey
  = AdjacencyKey
  { patternA  :: PatternIndex
  , patternB  :: PatternIndex
  , direction :: Direction
  }
  deriving (Eq, Ord, Show)

newtype AdjacencyRules = AdjacencyRules { getAdjacencyRules :: Map AdjacencyKey Bool }
  deriving (Eq, Show)

emptyAdjacencyRules :: AdjacencyRules
emptyAdjacencyRules = AdjacencyRules Map.empty

allow :: PatternIndex -> PatternIndex -> Direction -> AdjacencyRules -> AdjacencyRules
allow pA pB dir = AdjacencyRules . Map.insert (AdjacencyKey pA pB dir) True . getAdjacencyRules

disallow :: PatternIndex -> PatternIndex -> Direction -> AdjacencyRules -> AdjacencyRules
disallow pA pB dir = AdjacencyRules . Map.insert (AdjacencyKey pA pB dir) False . getAdjacencyRules

allowed :: PatternIndex -> PatternIndex -> Direction -> AdjacencyRules -> Maybe Bool
allowed pA pB dir = Map.lookup (AdjacencyKey pA pB dir) . getAdjacencyRules

generateAdjacencyRules :: Eq a => [Pattern a] -> AdjacencyRules
generateAdjacencyRules patterns' =
  let patternIndices = [ (x, y) | x <- [0..length patterns' - 1], y <- [0..length patterns' - 1]]
  in List.foldl' addRule emptyAdjacencyRules patternIndices
  where
    addRule :: AdjacencyRules -> (PatternIndex, PatternIndex) -> AdjacencyRules
    addRule rs (aIx, bIx) =
      foldl' (addRuleFor aIx bIx) rs [Up, Right', Down, Left']

    addRuleFor
      :: PatternIndex
      -> PatternIndex
      -> AdjacencyRules
      -> Direction
      -> AdjacencyRules
    addRuleFor aIx bIx rs' d =
      let patternA = patterns' List.!! aIx
          patternB = patterns' List.!! bIx
      in if overlaps patternA patternB d
         then allow aIx bIx d rs'
         else disallow aIx bIx d rs'

newtype ColorMap a = ColorMap { getColorMap :: Map PatternIndex a }
  deriving (Eq, Show)

emptyColorMap :: ColorMap a
emptyColorMap = ColorMap Map.empty

generateColorMap :: [Pattern a] -> ColorMap a
generateColorMap = List.foldl' addPatternElement emptyColorMap . zip [0..]
  where
    addPatternElement :: ColorMap a -> (PatternIndex, Pattern a) -> ColorMap a
    addPatternElement cmap (pIdx, p)
      = ColorMap
      . Map.insert pIdx (topLeftOf p)
      . getColorMap
      $ cmap

    topLeftOf :: Pattern a -> a
    topLeftOf Pattern {..} = getPattern Array.! (0, 0)

data Cell
  = Cell
  { cellPossibilities        :: Array PatternIndex Bool
  , cellCollapsed            :: Maybe PatternIndex
  , cellTotalWeight          :: Float
  , cellSumOfWeightLogWeight :: ActualFloat
  , cellPatternEnablerCounts :: Array Int PatternEnablerCount -- ^ Indexed by PatternA pattern index
  }
  deriving (Eq, Show)

-- | Initialize a Cell starting with all patterns as possible values
-- for the Cell.
--
-- As the algorithm proceeds we will eliminate possibilities and
-- collapse cells.
mkCell :: PatternResult a -> FrequencyHints -> AdjacencyRules -> Cell
mkCell p@PatternResult {..} hints adjacencyRules
  = Cell
  { cellPossibilities        = allPossibilities
  , cellCollapsed            = Nothing
  , cellTotalWeight          = totalWeight allPossibilities hints
  , cellSumOfWeightLogWeight = sumOfWeightLogWeight allPossibilities hints
  , cellPatternEnablerCounts = mkCellPatternEnablerCount p adjacencyRules
  }
  where
    allPossibilities
      = Array.listArray (0, patternResultMaxIndex)
      $ repeat True

collapsed :: Cell -> Bool
collapsed cell
  | isJust cell.cellCollapsed = True
  | otherwise                 = False

-- TODO: rename or fix me... seems we've already "picked" the
-- remaining by collapsing before calling this.
removePossibility :: FrequencyHints -> PatternIndex -> Cell -> Either String Cell
removePossibility hints patternIx cell = do
  let remaining = cell.cellPossibilities -- Array.// [(patternIx, False)]
  maybeCollapsed <- checkCollapsed remaining
  pure
    $ Cell
    { cellPossibilities = remaining
    , cellCollapsed = maybeCollapsed
    , cellTotalWeight = totalWeight remaining hints
    , cellSumOfWeightLogWeight = sumOfWeightLogWeight remaining hints
    }

-- | Return the list of 'PatternIndex' that overlap with @patternCheck@.
--
-- This list of 'PatternIndex' is used to remove those patterns from
-- the input cell later on because the @patternCheck@ pattern has been
-- removed from a neighbouring cell.
notEnabled :: PatternIndex -> Direction -> AdjacencyRules -> Cell -> [PatternIndex]
notEnabled patternCheck dir rules cell =
  let possibilities = filter snd $ Array.assocs cell.cellPossibilities
  in List.nub [ fst p | p <- possibilities, isEnabledFor patternCheck dir rules p ]
  where
    isEnabledFor
      :: PatternIndex
      -> Direction
      -> AdjacencyRules
      -> (PatternIndex, Bool)
      -> Bool
    isEnabledFor _ _ _ (_, False) = False
    isEnabledFor patB d r (patA, _) =
      case (getAdjacencyRules r) Map.!? (AdjacencyKey patA patB d) of
        Nothing -> error "TODO (fixme): filterPossibility, invalid adjacency rules"
        Just v  -> v

checkCollapsed :: Array PatternIndex Bool -> Either String (Maybe PatternIndex)
checkCollapsed arr =
  case exactlyOne id arr of
    ExactlyNone -> Left "No remaining patterns"
    ExactlyOne ix -> Right (Just ix)
    ExactlyMore -> Right Nothing

totalPossibleTileFrequency :: Array PatternIndex Bool -> FrequencyHints -> Int
totalPossibleTileFrequency possibilities hints =
  foldl' sumPossibleCell 0 $ Array.assocs possibilities
  where
    sumPossibleCell :: Int -> (PatternIndex, Bool) -> Int
    sumPossibleCell count (ix, True) = count + relativeFrequency ix hints
    sumPossibleCell count (_, False) = count

totalWeight :: Array PatternIndex Bool -> FrequencyHints -> Float
totalWeight possibilities hints = fromIntegral $ totalPossibleTileFrequency possibilities hints

sumOfWeightLogWeight :: Array PatternIndex Bool -> FrequencyHints -> ActualFloat
sumOfWeightLogWeight possibilities hints
  = actualFloat . sum . map toLogWeight . Array.assocs $ possibilities
  where
    toLogWeight :: (PatternIndex, Bool) -> Float
    toLogWeight (ix, True) =
      let rf = relativeFrequency ix hints
      in fromIntegral rf * (fromIntegral $ log2 $ fromIntegral rf)
    toLogWeight (_, False) = 0

entropy :: Cell -> Float
entropy Cell {..} =
  (logBase 2.0 cellTotalWeight) - (cellSumOfWeightLogWeight.getActualFloat / cellTotalWeight)

newtype Grid = Grid { getCells :: Array (Int, Int) Cell }
  deriving (Eq, Show)

mkGrid :: Word -> Word -> PatternResult a -> FrequencyHints -> AdjacencyRules -> Grid
mkGrid w h patternResult freqHints adjacencyRules
  = Grid
  . Array.listArray ((0, 0), (fromIntegral w - 1, fromIntegral h - 1))
  $ repeat (mkCell patternResult freqHints adjacencyRules)

cellAt :: (Int, Int) -> Grid -> Maybe Cell
cellAt cellIx grid = getCells grid `maybeAt` cellIx

setCell :: (Int, Int) -> Cell -> Grid -> Grid
setCell cellIx cell grid =
  Grid $ grid.getCells Array.// [(cellIx, cell)]

newtype EntropyCell = EntropyCell (Float, (Int, Int))
  deriving (Eq, Show)

instance Ord EntropyCell where
  EntropyCell (x, _) `compare` EntropyCell (y, _) = x `compare` y

data RemovePattern
  = RemovePattern
  { propagateCellPatternIx :: Int -- ^ The pattern index to remove
  , propagateCellIx :: (Int, Int) -- ^ The cell index to remove the
  }
                                  -- pattern index from
  deriving (Eq, Ord, Show)

newtype PatternEnablerCount
  = PatternEnablerCount
  { getPatternEnablerCount :: Array Int Int
  }
  deriving (Eq, Show)

mkPatternEnablerCount :: PatternEnablerCount
mkPatternEnablerCount
  = PatternEnablerCount
  $ Array.listArray (0, length directions - 1) [0, 0, 0, 0]

incrementDirection :: PatternEnablerCount -> Direction -> PatternEnablerCount
incrementDirection counts dir =
  let count = counts.getPatternEnablerCount Array.! fromEnum dir
  in PatternEnablerCount
  $ counts.getPatternEnablerCount Array.// [(fromEnum dir, count + 1)]

decrementDirection :: PatternEnablerCount -> Direction -> PatternEnablerCount
decrementDirection counts dir =
  let count = counts.getPatternEnablerCount Array.! fromEnum dir
  in PatternEnablerCount
  $ counts.getPatternEnablerCount Array.// [(fromEnum dir, count - 1)]

containsZeroCount :: PatternEnablerCount -> Bool
containsZeroCount (PatternEnablerCount enablerCount) =
  any (== 0) $ Array.elems enablerCount

getEnablerCount :: PatternEnablerCount -> Direction -> Int
getEnablerCount (PatternEnablerCount enablerCount) dir =
  enablerCount Array.! fromEnum dir

mkCellPatternEnablerCount :: PatternResult a -> AdjacencyRules -> Array Int PatternEnablerCount
mkCellPatternEnablerCount patternResult adjacencyRules =
  Array.listArray (0, patternResult.patternResultMaxIndex)
  [ initPatternEnablerCount p | p <- [0..patternResult.patternResultMaxIndex] ]
  where
    initPatternEnablerCount :: Int -> PatternEnablerCount
    initPatternEnablerCount pIx = PatternEnablerCount $
      Array.listArray (0, length directions - 1)
      [ length $ compatible adjacencyRules pIx dir | dir <- directions ]

compatible :: AdjacencyRules -> PatternIndex -> Direction -> [PatternIndex]
compatible adjacencyRules patternIx dir
  = map (patternB . fst)
  . filter ((\(AdjacencyKey patA _ d) -> patA == patternIx && d == dir) . fst)
  . filter snd
  . Map.toList
  . getAdjacencyRules
  $ adjacencyRules

data WaveState
  = WaveState
  { waveStateGrid               :: Grid
  , waveStateRemainingCells     :: Word
  , waveStateFrequencyHints     :: FrequencyHints
  , waveStateAdjacencyRules     :: AdjacencyRules
  , waveStateGen                :: StdGen
  , waveStateCellEntropyList    :: MinHeap EntropyCell
  , waveStateRemovePatternStack :: [RemovePattern]
  -- TODO: REMOVE ME, DEBUGGING
  , _waveStateSeenRemovals :: Set RemovePattern
  }

mkWaveState :: Ord a => (Word, Word) -> Int -> PatternResult a -> WaveState
mkWaveState (gridW, gridH) seed patternResult =
  let freqHints = frequencyHints patternResult.patternResultPatterns
      adjacencyRules = generateAdjacencyRules patternResult.patternResultPatterns
      grid = mkGrid gridW gridH patternResult freqHints adjacencyRules
      gen = mkStdGen seed
      (entropyList, gen')
        = buildEntropyList gen (Array.assocs . getCells $ grid) Heap.empty
  in WaveState
     { waveStateGrid = grid
     , waveStateRemainingCells = gridW * gridH
     , waveStateFrequencyHints = freqHints
     , waveStateAdjacencyRules = generateAdjacencyRules patternResult.patternResultPatterns
     , waveStateGen = gen'
     , waveStateCellEntropyList = entropyList
     , waveStateRemovePatternStack = []
     , _waveStateSeenRemovals = Set.empty
     }
  where
    buildEntropyList
      :: StdGen
      -> [((Int, Int), Cell)]
      -> MinHeap EntropyCell
      -> (MinHeap EntropyCell, StdGen)
    buildEntropyList gen [] accHeap = (accHeap, gen)
    buildEntropyList gen ((cellIx, cell):cells) accHeap =
      let (noise, gen') = uniformR (0, 1 :: Float) gen
          accHeap'
            = Heap.insert (EntropyCell (entropy cell + noise, cellIx)) accHeap
      in buildEntropyList gen' cells accHeap'

pushRemoveStack :: RemovePattern -> State WaveState ()
pushRemoveStack v
  = modify'
  $ \s -> s { waveStateRemovePatternStack = v : s.waveStateRemovePatternStack }

popRemoveStack :: State WaveState (Maybe RemovePattern)
popRemoveStack = do
  stack <- gets waveStateRemovePatternStack
  case stack of
    [] -> pure Nothing
    (x:xs) -> do
      modify' $ \s -> s { waveStateRemovePatternStack = xs }
      pure $ Just x

runWave :: WaveState -> State WaveState a -> Grid
runWave initState wave =
  let finalState = execState wave initState
  in finalState.waveStateGrid

-- For some initialized output grid of cells:
--   while there are uncollapsed cells:
--     choose a cell with the lowest entropy:
--        collapse it (picking a pattern from the possible patterns)
--        propagate the choice of that cell out to neighbours (across the wave)
--
-- Contradiction: when you pick a cell that has no possible patterns to collapse to.
--   Solutions: give up and start again or backtracking? maybe others?

collapseWave :: State WaveState ()
collapseWave = do
  remainingCells <- gets waveStateRemainingCells
  when (remainingCells > 0) $ do
    propagateCollapse
    collapseWave

propagateCollapse :: State WaveState ()
propagateCollapse = do
  cellIx <- chooseCell
  collapseAt cellIx
  propagate

isCollapsedAt :: (Int, Int) -> State WaveState Bool
isCollapsedAt cellIx = do
  cell <- getCellAt cellIx
  pure $ collapsed cell

collapseAt :: (Int, Int) -> State WaveState ()
collapseAt cellIx = do
  Debug.traceM $ "collapseAt: " ++ show cellIx
  WaveState {..} <- get
  let cells = getCells waveStateGrid
  case cells `maybeAt` cellIx of
    Nothing -> error $ "Invalid grid index in collapseAt: " ++ show cellIx
    Just cell -> do
      collapsedCell <- collapseCell cell
      let newGrid
            = Grid
            $ cells Array.// [(cellIx, collapsedCell)]
          patternStack'
            = map (flip RemovePattern cellIx)
            $ impossiblePatterns collapsedCell.cellPossibilities
      modify $ \s -> s { waveStateGrid = newGrid
                       , waveStateRemainingCells = s.waveStateRemainingCells - 1
                       , waveStateRemovePatternStack = patternStack'
                       }
  where
    collapseCell :: Cell -> State WaveState Cell
    collapseCell c = do
      patternIx <- pickPatternIx c
      pure
        $ c
        { cellPossibilities = collapseCellPossibilities patternIx c.cellPossibilities
        , cellCollapsed = Just patternIx
        }

    pickPatternIx :: Cell -> State WaveState PatternIndex
    pickPatternIx Cell {..} = do
      remaining <- randBetween 0 $ floor cellTotalWeight
      let ps = possiblePatterns cellPossibilities
      go remaining ps

    go :: Int -> [PatternIndex] -> State WaveState PatternIndex
    go _ [] = error "Couldn't find a pattern to collapse to."
    go remainder (p:ps) = do
      hints <- gets waveStateFrequencyHints
      let weight = relativeFrequency p hints
      if remainder <= weight
        then pure p
        else go (remainder - weight) ps

    possiblePatterns :: Array PatternIndex Bool -> [PatternIndex]
    possiblePatterns = map fst . filter snd . Array.assocs

    impossiblePatterns :: Array PatternIndex Bool -> [PatternIndex]
    impossiblePatterns = map fst . filter (not . snd) . Array.assocs

    collapseCellPossibilities
      :: PatternIndex
      -> Array PatternIndex Bool
      -> Array PatternIndex Bool
    collapseCellPossibilities pIx arr =
      let updatedArrayAssocs = map (keepIfMatches pIx) . Array.assocs $ arr
      in arr Array.// updatedArrayAssocs

    keepIfMatches pIx (pIx', _)
      | pIx == pIx' = (pIx', True)
      | otherwise   = (pIx', False)

-- while the patternRemovalStack is not empty:
--   (pattern to remove, cellIx) <- pop the top
--   for each direction:
--     modify the cell at cellIx + direction: eliminate the pattern from the set of possible patterns based on the adjacency rules
--     for each direction:
--       get set of enabled cells to the one we're looking at (based on the adjacency rules)
--       push those as removals to the pattern removal stack
--     update the cell's entropy + entropy cache
--     insert the cell into the heap list
--
-- Note: we can detect contradiction when the set of enabled cells is empty due to a prior removal.

propagate :: State WaveState ()
propagate = do
  removal <- popRemoveStack
  adjacencyRules <- gets waveStateAdjacencyRules
  case removal of
    Nothing -> pure ()
    Just removePattern -> do
      forM_ directions $ \dir -> do
        grid <- gets waveStateGrid
        let neighbourCoord = neighbourForDirection grid removePattern.propagateCellIx dir
        neighbourCell <- getCellAt neighbourCoord
        forM_ (compatible adjacencyRules removePattern.propagateCellPatternIx dir) $ \compatiblePattern -> do
          let enablerCounts = neighbourCell.cellPatternEnablerCounts Array.! compatiblePattern
          when (getEnablerCount enablerCounts dir == 1 && (not $ containsZeroCount enablerCounts)) $ do
            modifyCellAt neighbourCoord (removePatternFromCell compatiblePattern)
            -- possibly do something about contradiction?
            addEntropyCell neighbourCoord
            pushRemovals [RemovePattern compatiblePattern neighbourCoord]
          modifyCellAt neighbourCoord
            (decrementNeighbourEnablerCounts compatiblePattern enablerCounts dir)
      propagate
  where
    decrementNeighbourEnablerCounts
      :: PatternIndex
      -> PatternEnablerCount
      -> Direction
      -> Cell
      -> Either String Cell
    decrementNeighbourEnablerCounts patternIx enablerCounts dir cell =
      let cell' = cell
                  { cellPatternEnablerCounts =
                    cell.cellPatternEnablerCounts Array.// [(patternIx, decrementDirection enablerCounts dir)]
                  }
      in Right cell'

    removePatternFromCell :: PatternIndex -> Cell -> Either String Cell
    removePatternFromCell patternIx cell
      = Right cell
      { cellPossibilities = cell.cellPossibilities Array.// [(patternIx, False)]
      }

-- 1. The adjacency rules should always hold
-- 2. The remaining possible cell values of my neighbours must always
--    be legal with respect to the adjacency rules

-- Remove patterns from the set of possible patterns for this PatternIndex
-- based on the AdjacencyRules
--
-- Get set of not-enabled Patterns for the cells in each direction:
--   add those patterns to the pattern removal stack
eliminate :: RemovePattern -> State WaveState ()
eliminate RemovePattern {..} = do
  freqHints <- gets waveStateFrequencyHints
  modifyCellAt propagateCellIx
    $ removePossibility freqHints propagateCellPatternIx
  neighborRemovals <- forM directions $
    getNeighborRemovals propagateCellIx propagateCellPatternIx
  pushRemovals $ join neighborRemovals

getNeighborRemovals
  :: (Int, Int)
  -> PatternIndex
  -> Direction
  -> State WaveState [RemovePattern]
getNeighborRemovals baseCellIx patternIx dir = do
  grid <- gets waveStateGrid
  let removeCellIx = fromDirection grid baseCellIx dir
  cell <- getCellAt removeCellIx
  adjacencyRules <- gets waveStateAdjacencyRules
  pure
    . map (flip RemovePattern removeCellIx) -- <-- Perhaps right here
    $ notEnabled patternIx dir adjacencyRules cell

pushRemovals :: [RemovePattern] -> State WaveState ()
pushRemovals removals = do
  modify'
    $ \s -> s
    { waveStateRemovePatternStack = s.waveStateRemovePatternStack ++ removals
    }

modifyCellAt :: (Int, Int) -> (Cell -> Either String Cell) -> State WaveState ()
modifyCellAt cellIx f = do
  cell <- getCellAt cellIx
  case f cell of
    Left err -> error err
    Right cell' -> do
      when (cellEntropyChanged cell cell') $
        addEntropyCell cellIx
      putCellAt cellIx cell'
  where
    cellEntropyChanged :: Cell -> Cell -> Bool
    cellEntropyChanged (Cell _ _ _ weightX logWeightX) (Cell _ _ _ weightY logWeightY)
      | weightX == weightY && logWeightX == logWeightY = False
      | otherwise                                      = True

chooseCell :: State WaveState (Int, Int)
chooseCell = do
  entropyCells <- gets waveStateCellEntropyList
  case Heap.view entropyCells of
    Nothing -> error "Shouldn't be able to get here!" -- TODO (james): error handling over StateT?
    Just (EntropyCell (_, cellIx), remainingEntropyCells) -> do
      modify' $ \s -> s { waveStateCellEntropyList = remainingEntropyCells }
      cellCollapsed <- isCollapsedAt cellIx
      if cellCollapsed
      then chooseCell
      else pure cellIx

getCellAt :: (Int, Int) -> State WaveState Cell
getCellAt cellIx = do
  grid <- gets waveStateGrid
  case cellAt cellIx grid of
    Nothing -> error "Fix me: getCellAt"
    Just cell -> pure cell

putCellAt :: (Int, Int) -> Cell -> State WaveState ()
putCellAt cellIx cell =
  modify' $ \s -> s { waveStateGrid = setCell cellIx cell s.waveStateGrid }

addEntropyCell :: (Int, Int) -> State WaveState ()
addEntropyCell cellIx = do
  cell <- getCellAt cellIx
  entropyCells <- gets waveStateCellEntropyList
  noise <- randNoise
  let entropyCells'
        = EntropyCell (noise + entropy cell, cellIx) `Heap.insert` entropyCells
  modify' $ \s -> s { waveStateCellEntropyList = entropyCells' }

maybeAt :: (Array.Ix i, Show i, Show e) => Array i e -> i -> Maybe e
maybeAt arr ix
  | Array.inRange (Array.bounds arr) ix = pure $ arr Array.! ix
  | otherwise = Nothing

randBetween :: Int -> Int -> State WaveState Int
randBetween lo hi = do
  gen <- gets waveStateGen
  let (x, gen') = uniformR (lo :: Int, hi :: Int) gen
  modify $ \s -> s { waveStateGen = gen' }
  pure x

randNoise :: State WaveState Float
randNoise = do
  gen <- gets waveStateGen
  let (x, gen') = uniformR (0 :: Float, 1) gen
  modify $ \s -> s { waveStateGen = gen' }
  pure x

data ExactlyOneResult a
  = ExactlyNone
  | ExactlyOne a
  | ExactlyMore
  deriving (Eq, Show)

exactlyOne :: Array.Ix ix => (a -> Bool) -> Array ix a -> ExactlyOneResult ix
exactlyOne f = List.foldl' exactly ExactlyNone . Array.assocs
  where
    exactly ExactlyNone (i, v) | f v = ExactlyOne i
    exactly e@ExactlyNone _ = e
    exactly (ExactlyOne _) (_, v) | f v = ExactlyMore
    exactly e@(ExactlyOne _) _ = e
    exactly ExactlyMore _ = ExactlyMore

fromDirection :: Grid -> (Int, Int) -> Direction -> (Int, Int)
fromDirection grid (x, y) dir =
  let (gridW, gridH) = snd . Array.bounds $ getCells grid
  in case dir of
    Up ->
      let y' = y - 1
      in (x, if y' < 0 then gridH else y')
    Right' ->
      let x' = x + 1
      in (if x' >= gridW + 1 then 0 else x', y)
    Down ->
      let y' = y + 1
      in (x, if y' >= gridH + 1 then 0 else y')
    Left' ->
      let x' = x - 1
      in (if x' < 0 then gridW else x', y)

neighbourForDirection :: Grid -> (Int, Int) -> Direction -> (Int, Int)
neighbourForDirection grid (x, y) dir =
  let (gridW, gridH) = snd . Array.bounds $ getCells grid
  in case dir of
    Up ->
      let y' = y + 1
      in (x, if y' >= gridH + 1 then 0 else y')
    Right' ->
      let x' = x - 1
      in (if x' < 0 then gridW else x', y)
    Down ->
      let y' = y - 1
      in (x, if y' < 0 then gridH else y')
    Left' ->
      let x' = x + 1
      in (if x' >= gridW + 1 then 0 else x', y)

newtype ActualFloat = ActualFloat { getActualFloat :: Float }
  deriving (Eq, Show)

actualFloat :: Float -> ActualFloat
actualFloat x
  | isNaN x = error "Expected an actual float, found a NaN"
  | otherwise = ActualFloat x

log2 :: Integer -> Int
log2 = imLog 2

imLog b x = if x < b then 0 else (x `div` b^l) `doDiv` l
  where
    l = 2 * imLog (b * b) x
    doDiv x' l' = if x' < b then l' else (x' `div` b) `doDiv` (l' + 1)
