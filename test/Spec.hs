{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad
import qualified Data.Array as Array
import qualified Data.Map.Strict as Map
import Test.Hspec
import Test.Hspec.QuickCheck
import Algorithm.WaveFunctionCollapse

main :: IO ()
main = hspec $ do
  describe "patterns" $ do
    prop "sum of sub patterns" $ \(tex :: Texture Int) ->
      (length $ patterns tex 3) `shouldBe` fromIntegral ((textureSize tex) * (textureSize tex))

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
      (frequencyHints $ patterns tex 3) `shouldSatisfy` Map.foldr (\x r -> r && x >= 1) True

  describe "AdjacencyRules" $ do
    prop "always allowed" $ \x y d -> do
      let rules = emptyAdjacencyRules
      (allowed x y d . allow x y d $ rules) `shouldBe` Just True
