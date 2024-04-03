{-# LANGUAGE ScopedTypeVariables #-}

import Test.Hspec
import Test.Hspec.QuickCheck
import Algorithm.WaveFunctionCollapse

main :: IO ()
main = hspec $ do
  describe "patterns" $ do
    prop "all overlapping patterns" $ \(tex :: Texture Int) ->
      (length $ patterns tex 3) `shouldBe` fromIntegral ((textureSize tex) * (textureSize tex))
