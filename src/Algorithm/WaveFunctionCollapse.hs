{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Algorithm.WaveFunctionCollapse where

import Data.Array (Array, (!))
import qualified Data.Array as Array
import Data.Foldable
import Data.Maybe
import Test.QuickCheck

data Pattern a
  = Pattern
  { patternSize :: Word
  , getPattern  :: Array (Word, Word) a
  }
  deriving (Eq, Show)

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
    s <- chooseInt (1, 100)
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

patterns :: Texture a -> Word -> [Pattern a]
patterns texture subPatternSize
  | subPatternSize == 0 = []
  | otherwise =
    let upTo = textureSize texture - 1
    in [ extractPattern texture (x, y) subPatternSize
       | x :: Word <- [0..upTo], y :: Word <- [0..upTo]
       ]
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
  deriving (Eq, Show)

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
