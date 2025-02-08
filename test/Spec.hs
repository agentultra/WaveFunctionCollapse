{-# LANGUAGE LambdaCase#-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad
import qualified Data.Array as Array
import Data.List (foldl', sort)
import qualified Data.Heap as Heap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Test.Hspec
import Test.Hspec.QuickCheck
import Algorithm.WaveFunctionCollapse
import System.Random

import qualified Debug.Trace as Debug

main :: IO ()
main = hspec $ do
  describe "patterns" $ do
    prop "sum of sub patterns" $ \(tex :: Texture Int) ->
      (length . patternResultPatterns $ patterns tex 3) `shouldBe` fromIntegral ((textureSize tex) * (textureSize tex))

    context "Given a sub pattern from the 0,0 index of the source texture" $ do
      it "should return the matching values for the sub-pattern indices" $ do
        let tex = textureFromList @Int 4
                  [ 0, 1, 0, 0
                  , 0, 1, 0, 0
                  , 1, 1, 1, 1
                  , 0, 1, 0, 0
                  ]
            -- This only works for the top-left sub-pattern from 0,0
            -- in the source texture
            pat = head . patternResultPatterns $ patterns tex 3
            patIxs = Array.indices pat.getPattern          -- The pattern indices should
            texValues = foldl' (getValue tex) [] patIxs    -- coincide with the source
            getValue (Texture t) vs ix = t Array.! ix : vs -- texture indices

        texValues `shouldBe` Array.elems pat.getPattern

    context "Given an input texture" $ do
      it "should return distinct patterns" $ do
        let tex = textureFromList @Int 4
                  [ 0, 1, 0, 0
                  , 0, 1, 0, 0
                  , 1, 1, 1, 1
                  , 0, 1, 0, 0
                  ]
            allPatterns = patternResultPatterns $ patterns tex 3
            uniquePatterns = Set.toList . Set.fromList $ allPatterns
        sort allPatterns `shouldBe` sort uniquePatterns

  describe "Pattern" $ do
    prop "full clockwise rotation" $ \(pat :: Pattern Int) ->
      (clockwise . clockwise . clockwise . clockwise $ pat) `shouldBe` pat

  describe "overlaps" $ do
    context "A fill pattern" $ do
      it "should always overlap with itself in every direction" $ do
        let (Just fillPattern) = mkFillPattern (0 :: Int) 3
        forM_ [Up, Right', Down, Left'] $ \dir -> do
          overlaps fillPattern fillPattern dir `shouldBe` True

    context "Known overlapping patterns" $ do
      let p1 = Pattern @Int 3
             $ Array.listArray ((0,0), (2,2))
             [ 1, 1, 3
             , 1, 1, 3
             , 1, 1, 3
             ]
          p2 = Pattern @Int 3
             $ Array.listArray ((0,0), (2,2))
             [ 3, 1, 1
             , 3, 1, 1
             , 3, 1, 1
             ]
          p3 = Pattern @Int 3
             $ Array.listArray ((0,0), (2,2))
             [ 3, 2, 3
             , 3, 3, 3
             , 1, 1, 1
             ]
          p4 = Pattern @Int 3
             $ Array.listArray ((0,0), (2,2))
             [ 3, 2, 3
             , 3, 2, 3
             , 3, 3, 3
             ]
      it "should overlap on Up" $ do
        overlaps p1 p2 Up `shouldBe` True

      it "should overlap on Right'" $ do
        overlaps p3 p4 Right' `shouldBe` True

      it "should not overlap on Right'" $ do
        overlaps p3 p4 Left' `shouldBe` False

      it "should not overlap on any others" $ do
        forM_ [Right', Down, Left'] $ \dir -> do
          overlaps p1 p2 dir `shouldBe` False

  describe "frequencyMap" $ do
    xprop "minimum frequency map" $ \(tex :: Texture Int) -> do
      (getFrequencyHints . frequencyHints . patternResultPatterns $ patterns tex 3) `shouldSatisfy` Map.foldr (\x r -> r && x >= 1) True

  describe "AdjacencyRules" $ do
    prop "always allowed" $ \x y d -> do
      let rules = emptyAdjacencyRules
      (allowed x y d . allow x y d $ rules) `shouldBe` Just True

  describe "collapseAt" $ do
    context "Given a Grid" $ do
      -- TODO (james): maybe text with arbitrary textures? or a better texture?
      let testTexture = mkTexture (0 :: Int) 5
          patternResult = patterns testTexture 3
          hints = frequencyHints patternResult.patternResultPatterns
          rules = generateAdjacencyRules patternResult.patternResultPatterns
          grid = mkGrid 10 10 patternResult hints rules
          initWaveState
            = WaveState
            { waveStateGrid = grid
            , waveStateFrequencyHints =
                frequencyHints (patternResult.patternResultPatterns)
            , waveStateGen = mkStdGen 100
            , waveStateCellEntropyList = Heap.empty
            , waveStateRemainingCells = 0
            , waveStateAdjacencyRules = emptyAdjacencyRules
            , waveStateRemovePatternStack = []
            }
      it "should collapse to a single pattern" $ do
        let resultState = runWave initWaveState $ collapseAt (0, 0)
        case cellAt (0, 0) resultState.waveStateGrid of
          Nothing -> fail "Missing expected cell"
          Just cell -> collapsed cell `shouldBe` True

  describe "notEnabled" $ do
    let rules = AdjacencyRules
                $ Map.fromList
                [ ((AdjacencyKey 0 1 Up), True) -- HERE
                , ((AdjacencyKey 0 1 Down), False)
                , ((AdjacencyKey 0 1 Left'), False)
                , ((AdjacencyKey 0 1 Right'), False)
                , ((AdjacencyKey 0 2 Up), True)
                , ((AdjacencyKey 0 2 Down), False)
                , ((AdjacencyKey 0 2 Left'), False)
                , ((AdjacencyKey 0 2 Right'), False)
                , ((AdjacencyKey 1 2 Up), False)
                , ((AdjacencyKey 1 2 Down), False)
                , ((AdjacencyKey 1 2 Left'), False)
                , ((AdjacencyKey 1 2 Right'), False)
                , ((AdjacencyKey 2 1 Up), True)  -- HERE
                , ((AdjacencyKey 2 1 Down), False)
                , ((AdjacencyKey 2 1 Left'), False)
                , ((AdjacencyKey 2 1 Right'), False)
                ]
    context "Given Cell with two possibilities, one possible" $ do
      let cell
            = Left Cell
            { cellPossibilities = Array.listArray (0, 2) [True, False, True]
            , cellTotalWeight = 0.0
            , cellSumOfWeightLogWeight = ActualFloat 0.0
            , cellPatternEnablerCounts = Array.listArray (0, -1) []

            }
      it "should return possibilies that are not enabled" $ do
        let toRemove = notEnabled 1 Up rules cell
        toRemove `shouldBe` [0, 2]

      it "should keep enabled possibilities" $ do
        let toRemove = notEnabled 1 Down rules cell
        toRemove `shouldBe` []

    context "Given Cell with two possibilies, all possible" $ do
      let cell
            = Left Cell
            { cellPossibilities = Array.listArray (0, 1) [True, True]
            , cellTotalWeight = 0.0
            , cellSumOfWeightLogWeight = ActualFloat 0.0
            , cellPatternEnablerCounts = Array.listArray (0, -1) []
            }
      xit "should not consider rules that aren't allowed" $ do
        let toRemove = notEnabled 1 Down rules cell
        toRemove `shouldBe` []

      xit "should return pattern indices that are no longer enabled" $ do
        let toRemove = notEnabled 1 Up rules cell
        toRemove `shouldBe` [0]

  describe "fromDirection" $ do
    let inputTexture
            = textureFromList @Int 4
            [ 0, 1, 0, 0
            , 0, 1, 0, 0
            , 1, 1, 1, 1
            , 0, 1, 0, 0
            ]
        patternResult = patterns inputTexture 3
        fHints = frequencyHints $ patternResult.patternResultPatterns
        rules = generateAdjacencyRules patternResult.patternResultPatterns
        testGrid = mkGrid 3 3 patternResult fHints rules
        dummyCell
          = Left Cell
          { cellPossibilities = Array.listArray (0, 0) [False]
          , cellTotalWeight = 0.0
          , cellSumOfWeightLogWeight = ActualFloat 0.0
          , cellPatternEnablerCounts = Array.listArray (0, -1) []
          }
        expectedCell = \case
            Up     -> (1,0)
            Right' -> (2,1)
            Down   -> (1,2)
            Left'  -> (0,1)
    forM_ directions $ \dir -> do
      it ("returns the dummy cell in direction: " ++ show dir) $ do
        let cellFromDir = fromDirection testGrid (1,1) dir
            testGrid' = setCell (expectedCell dir) dummyCell testGrid
        cellAt cellFromDir testGrid' `shouldBe` cellAt (expectedCell dir) testGrid'

    it "Up wraps around to bottom" $ do
      let cellFromDir = fromDirection testGrid (0,0) Up
          testGrid' = setCell (0,2) dummyCell testGrid
      cellAt cellFromDir testGrid' `shouldBe` cellAt (0,2) testGrid'

    it "Down wraps around to top" $ do
      let cellFromDir = fromDirection testGrid (0,2) Down
          testGrid' = setCell (0,0) dummyCell testGrid
      cellAt cellFromDir testGrid' `shouldBe` cellAt (0,0) testGrid'

    it "Right' wraps around to left" $ do
      let cellFromDir = fromDirection testGrid (2,0) Right'
          testGrid' = setCell (0,0) dummyCell testGrid
      cellAt cellFromDir testGrid' `shouldBe` cellAt (0,0) testGrid'

    it "Left' wraps around to right" $ do
      let cellFromDir = fromDirection testGrid (0,0) Left'
          testGrid' = setCell (2,0) dummyCell testGrid
      cellAt cellFromDir testGrid' `shouldBe` cellAt (2,0) testGrid'

  describe "neighbourForDirection" $ do
    let inputTexture
            = textureFromList @Int 4
            [ 0, 1, 0, 0
            , 0, 1, 0, 0
            , 1, 1, 1, 1
            , 0, 1, 0, 0
            ]
        patternResult = patterns inputTexture 3
        fHints = frequencyHints $ patternResult.patternResultPatterns
        rules = generateAdjacencyRules patternResult.patternResultPatterns
        testGrid = mkGrid 3 3 patternResult fHints rules
        dummyCell
          = Left Cell
          { cellPossibilities = Array.listArray (0, 0) [False]
          , cellTotalWeight = 0.0
          , cellSumOfWeightLogWeight = ActualFloat 0.0
          , cellPatternEnablerCounts = Array.listArray (0, -1) []
          }
        expectedCell = \case
            Up     -> (1,2)
            Right' -> (0,1)
            Down   -> (1,0)
            Left'  -> (2,1)
    forM_ directions $ \dir -> do
      it ("returns the dummy cell in direction: " ++ show dir) $ do
        let cellFromDir = neighbourForDirection testGrid (1,1) dir
            testGrid' = setCell (expectedCell dir) dummyCell testGrid
        cellAt cellFromDir testGrid' `shouldBe` cellAt (expectedCell dir) testGrid'

    it "Up wraps around to top" $ do
      let cellFromDir = neighbourForDirection testGrid (0,2) Up
          testGrid' = setCell (0,0) dummyCell testGrid
      cellAt cellFromDir testGrid' `shouldBe` cellAt (0,0) testGrid'

    it "Down wraps around to bottom" $ do
      let cellFromDir = neighbourForDirection testGrid (0,0) Down
          testGrid' = setCell (0,2) dummyCell testGrid
      cellAt cellFromDir testGrid' `shouldBe` cellAt (0,2) testGrid'

    it "Right' wraps around to right" $ do
      let cellFromDir = neighbourForDirection testGrid (0,0) Right'
          testGrid' = setCell (2,0) dummyCell testGrid
      cellAt cellFromDir testGrid' `shouldBe` cellAt (2,0) testGrid'

    it "Left' wraps around to left" $ do
      let cellFromDir = neighbourForDirection testGrid (2,0) Left'
          testGrid' = setCell (0,0) dummyCell testGrid
      cellAt cellFromDir testGrid' `shouldBe` cellAt (0,0) testGrid'

    it "should get in bounds" $ do
      let cellFromDir = neighbourForDirection testGrid (1,3) Up
          testGrid' = setCell (1,0) dummyCell testGrid
      cellAt cellFromDir testGrid' `shouldBe` cellAt (1,0) testGrid'

  describe "runWave" $ do
    it "returns a Grid changed from what we start with" $ do
      let inputTexture
            = textureFromList @Int 4
            [ 0, 1, 0, 0
            , 0, 1, 0, 0
            , 1, 1, 1, 1
            , 0, 1, 0, 0
            ]
          patternResult = patterns inputTexture 3
          seed = 100
          initWaveState = mkWaveState (10, 10) seed patternResult
          finalState = runWave initWaveState collapseWave
      Debug.traceM ("here: " ++ show (head patternResult.patternResultPatterns))
      initWaveState.waveStateGrid /= finalState.waveStateGrid `shouldBe` True
