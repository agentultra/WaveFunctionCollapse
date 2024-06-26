{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Algorithm.WaveFunctionCollapse where

import Control.Monad
import Control.Monad.State.Strict
import Data.Array (Array, (!))
import qualified Data.Array as Array
import Data.Bifunctor
import Data.Heap (MinHeap)
import qualified Data.Heap as Heap
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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
relativeFrequency ix = maybe 0 id . Map.lookup ix . getFrequencyHints

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
         else rs'

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
  , cellSumOfWeightLogWeight :: Float
  }
  deriving (Eq, Show)

-- | Initialize a Cell starting with all patterns as possible values
-- for the Cell.
--
-- As the algorithm proceeds we will eliminate possibilities and
-- collapse cells.
mkCell :: PatternResult a -> FrequencyHints -> Cell
mkCell PatternResult {..} hints
  = Cell
  { cellPossibilities        = allPossibilities
  , cellCollapsed            = Nothing
  , cellTotalWeight          = totalWeight allPossibilities hints
  , cellSumOfWeightLogWeight = sumOfWeightLogWeight allPossibilities hints
  }
  where
    allPossibilities
      = Array.listArray (0, patternResultMaxIndex)
      $ repeat True

collapsed :: Cell -> Bool
collapsed cell
  | isJust cell.cellCollapsed = True
  | otherwise                 = False

-- NOTE: remember to call removePossibility when eliminating
removePossibility :: FrequencyHints -> PatternIndex -> Cell -> Either String Cell
removePossibility hints patternIx cell = do
  -- Remove PatternIndex from cell possibilities
  -- Recalculate totalWeight and sumOfWeightLogWeight
  let remaining = cell.cellPossibilities Array.// [(patternIx, False)]
  maybeCollapsed <- checkCollapsed remaining
  pure
    $ Cell
    { cellPossibilities = remaining
    , cellCollapsed = maybeCollapsed
    , cellTotalWeight = totalWeight remaining hints
    , cellSumOfWeightLogWeight = sumOfWeightLogWeight remaining hints
    }

-- | Get the list of pattern indices that are /not enabled/ for the input set of pattern indices given the AdjacencyRules
--
-- /Enabled/ means that the possible value in the input cell doesn't
-- match any rule involving the input pattern for the given direction,
-- where the input pattern is the target and possible value is the
-- source.
notEnabled :: NonEmpty PatternIndex -> Direction -> AdjacencyRules -> Cell -> [PatternIndex]
notEnabled patternIxBs dir rules cell =
  let possibilities = filter snd $ Array.assocs cell.cellPossibilities
  in List.nub [ fst p | patternCheck <- NE.toList patternIxBs, p <- possibilities, isNotEnabled patternCheck dir rules p ]
  where
    isNotEnabled
      :: PatternIndex
      -> Direction
      -> AdjacencyRules
      -> (PatternIndex, Bool)
      -> Bool
    isNotEnabled _ _ _ (_, False) = error "NEVER HIT ME!"
    isNotEnabled patB d r (patA, _) =
      case (getAdjacencyRules r) Map.!? (AdjacencyKey patA patB d) of
        Nothing -> error "TODO (fixme): filterPossibility, invalid adjacency rules"
        Just v  -> not v

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

sumOfWeightLogWeight :: Array PatternIndex Bool -> FrequencyHints -> Float
sumOfWeightLogWeight possibilities hints
  = sum . map toLogWeight . Array.assocs $ possibilities
  where
    toLogWeight :: (PatternIndex, Bool) -> Float
    toLogWeight (ix, True) =
      let rf = fromIntegral $ relativeFrequency ix hints
      in rf * (logBase 2.0 rf)
    toLogWeight (_, False) = 0

entropy :: Cell -> Float
entropy Cell {..} =
  (logBase 2.0 cellTotalWeight) - (cellSumOfWeightLogWeight / cellTotalWeight)

newtype Grid = Grid { getCells :: Array (Int, Int) Cell }
  deriving (Eq, Show)

mkGrid :: Ord a => Word -> Word -> PatternResult a -> FrequencyHints -> Grid
mkGrid w h patternResult freqHints
  = Grid
  . Array.listArray ((0, 0), (fromIntegral w - 1, fromIntegral h - 1))
  $ repeat (mkCell patternResult freqHints)

cellAt :: (Int, Int) -> Grid -> Maybe Cell
cellAt cellIx grid = (getCells grid) `maybeAt` cellIx

newtype EntropyCell = EntropyCell (Float, (Int, Int))
  deriving (Eq, Show)

instance Ord EntropyCell where
  EntropyCell (x, _) `compare` EntropyCell (y, _) = x `compare` y

data RemovePattern
  = RemovePattern
  { propagateCellPatternIx :: Int -- ^ The pattern index to remove
  , propagateCellIx :: (Int, Int) -- ^ The cell index to remove the
                                  -- pattern index from
  }
  deriving (Eq, Show)

data WaveState
  = WaveState
  { waveStateGrid               :: Grid
  , waveStateRemainingCells     :: Word
  , waveStateFrequencyHints     :: FrequencyHints
  , waveStateAdjacencyRules     :: AdjacencyRules
  , waveStateGen                :: StdGen
  , waveStateCellEntropyList    :: MinHeap EntropyCell
  , waveStateRemovePatternStack :: [RemovePattern]
  }

mkWaveState :: (Eq a, Ord a) => (Word, Word) -> Int -> PatternResult a -> WaveState
mkWaveState (gridW, gridH) seed patternResult =
  let freqHints = frequencyHints patternResult.patternResultPatterns
      grid = mkGrid gridW gridH patternResult freqHints
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
  when (remainingCells > 0) propagateCollapse

propagateCollapse :: State WaveState ()
propagateCollapse = do
  cellIx <- chooseCell
  collapseAt cellIx
  propagate
  pure ()

isCollapsedAt :: (Int, Int) -> State WaveState Bool
isCollapsedAt cellIx = do
  cell <- getCellAt cellIx
  pure $ collapsed cell

collapseAt :: (Int, Int) -> State WaveState ()
collapseAt cellIx = do
  WaveState {..} <- get
  let cells = getCells waveStateGrid
  case cells `maybeAt` cellIx of
    Nothing -> error $ "Invalid grid index in collapseAt: " ++ show cellIx
    Just cell -> do
      collapsedCell <- collapseCell cell
      let newGrid
            = Grid
            $ cells Array.// [(cellIx, collapsedCell)]
          (Just collapsedPattern) = collapsedCell.cellCollapsed
          -- TODO: initialize the patternStack with the pattern
          -- indices to remove at each neighbour based on the
          -- adjacency rules
          patternStack' = [ RemovePattern collapsedPattern cellIx ]
      modify $ \s -> s { waveStateGrid = newGrid
                       , waveStateRemainingCells = s.waveStateRemainingCells - 1
                       , waveStateRemovePatternStack = patternStack'
                       }
  where
    collapseCell :: Cell -> State WaveState Cell
    collapseCell c = do
      patternIx <- pickPatternIx c
      pure
        $ Cell
        { cellPossibilities = collapseCellPossibilities patternIx c.cellPossibilities
        , cellCollapsed = Just patternIx
        , cellTotalWeight = 0.0
        , cellSumOfWeightLogWeight = 0.0
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
      if remainder >= weight
        then go (remainder - weight) ps
        else pure p

    possiblePatterns :: Array PatternIndex Bool -> [PatternIndex]
    possiblePatterns = map fst . filter snd . Array.assocs

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
  case removal of
    Nothing -> pure ()
    Just removePattern -> do
      eliminate removePattern

-- 1. The adjacency rules should always hold
-- 2. The remaining possible cell values of my neighbours must always
--    be legal with respect to the adjacency rules

-- Remove patterns from the set of possible patterns for this PatternIndex
-- based on the AdjacencyRules
--
-- Get set of not-enabled Patterns for the cells in each direction:
--   add those patterns to the pattern removal stack
eliminate :: RemovePattern -> State WaveState ()
eliminate RemovePattern {..} = undefined

modifyCellAt :: (Int, Int) -> (Cell -> Cell) -> State WaveState ()
modifyCellAt = undefined

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

addEntropyCell :: (Int, Int) -> State WaveState ()
addEntropyCell cellIx = do
  cell <- getCellAt cellIx
  entropyCells <- gets waveStateCellEntropyList
  noise <- randNoise
  let entropyCells'
        = EntropyCell (noise + entropy cell, cellIx) `Heap.insert` entropyCells
  modify' $ \s -> s { waveStateCellEntropyList = entropyCells' }

maybeAt :: Array.Ix i => Array i e -> i -> Maybe e
maybeAt arr ix
  | Array.inRange (Array.bounds arr) ix = Just $ arr Array.! ix
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
