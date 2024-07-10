{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad
import qualified Data.Array as Array
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Heap as Heap
import qualified Data.Map.Strict as Map
import Test.Hspec
import Test.Hspec.QuickCheck
import Algorithm.WaveFunctionCollapse
import System.Random

main :: IO ()
main = hspec $ do
  describe "patterns" $ do
    prop "sum of sub patterns" $ \(tex :: Texture Int) ->
      (length . patternResultPatterns $ patterns tex 3) `shouldBe` fromIntegral ((textureSize tex) * (textureSize tex))

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
      it "should overlap on Up" $ do
        overlaps p1 p2 Up `shouldBe` True

      it "should not overlap on any others" $ do
        forM_ [Right', Down, Left'] $ \dir -> do
          overlaps p1 p2 dir `shouldBe` False

  describe "frequencyMap" $ do
    prop "minimum frequency map" $ \(tex :: Texture Int) -> do
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
          grid = mkGrid 10 10 patternResult hints
          initWaveState
            = WaveState
            { waveStateGrid = grid
            , waveStateFrequencyHints =
                frequencyHints (patternResult.patternResultPatterns)
            , waveStateGen = mkStdGen 100
            , waveStateCellEntropyList = Heap.empty
            }
      it "should collapse to a single pattern" $ do
        let resultGrid = runWave initWaveState $ collapseAt (0, 0)
        case cellAt (0, 0) resultGrid of
          Nothing -> fail "Missing expected cell"
          Just cell -> collapsed cell `shouldBe` True

  describe "notEnabled" $ do
    let rules = AdjacencyRules
                $ Map.fromList
                [ ((AdjacencyKey 0 1 Up), True)
                , ((AdjacencyKey 0 1 Down), False)
                , ((AdjacencyKey 0 1 Left'), False)
                , ((AdjacencyKey 0 1 Right'), False)
                , ((AdjacencyKey 0 2 Up), True)
                , ((AdjacencyKey 0 2 Down), False)
                , ((AdjacencyKey 0 2 Left'), False)
                , ((AdjacencyKey 0 2 Right'), False)
                , ((AdjacencyKey 1 1 Up), False)
                , ((AdjacencyKey 1 1 Down), False)
                , ((AdjacencyKey 1 1 Left'), False)
                , ((AdjacencyKey 1 1 Right'), False)
                , ((AdjacencyKey 1 2 Up), False)
                , ((AdjacencyKey 1 2 Down), False)
                , ((AdjacencyKey 1 2 Left'), False)
                , ((AdjacencyKey 1 2 Right'), False)
                ]
    context "Given Cell with two possibilities, one possible" $ do
      let cell
            = Cell
            { cellPossibilities = Array.listArray (0, 1) [True, False]
            , cellCollapsed = Nothing
            , cellTotalWeight = 0.0
            , cellSumOfWeightLogWeight = 0.0
            }
      it "should return possibilies that are not enabled" $ do
        let toRemove = notEnabled 1 Up rules cell
        toRemove `shouldBe` [0]

      it "should keep enabled possibilities" $ do
        let toRemove = notEnabled 1 Down rules cell
        toRemove `shouldBe` []

    context "Given Cell with two possibilies, all possible" $ do
      let cell
            = Cell
            { cellPossibilities = Array.listArray (0, 1) [True, True]
            , cellCollapsed = Nothing
            , cellTotalWeight = 0.0
            , cellSumOfWeightLogWeight = 0.0
            }
      it "should not consider rules that aren't allowed" $ do
        let toRemove = notEnabled 1 Down rules cell
        toRemove `shouldBe` []

      it "should return pattern indices that are no longer enabled" $ do
        let toRemove = notEnabled 1 Up rules cell
        toRemove `shouldBe` [0]
