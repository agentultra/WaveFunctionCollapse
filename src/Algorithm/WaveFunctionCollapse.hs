{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Algorithm.WaveFunctionCollapse where

import Data.Array (Array, (!))
import qualified Data.Array as Array
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Foldable
import Data.Maybe
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
  deriving (Eq, Ord, Show)

instance Arbitrary Direction where
  arbitrary = oneof [pure Up, pure Right', pure Down, pure Left']

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

frequencyHints :: Ord a => [Pattern a] -> Map Int Int
frequencyHints ps = go Map.empty ps . zip [0..] $ ps
  where
    go :: Ord a => Map Int Int -> [Pattern a] -> [(Int, Pattern a)] -> Map Int Int
    go accum _ [] = accum
    go accum ps' ((ix,p):xs) = go (Map.insert ix (seen p ps') accum) ps' xs

    seen :: Eq a => Pattern a -> [Pattern a] -> Int
    seen x = List.foldl' (\count y -> if x == y then (count + 1) else count) 0

type PatternIndex = Int

data AdjacencyKey
  = AdjacencyKey
  { patternA  :: PatternIndex
  , patternB  :: PatternIndex
  , direction :: Direction
  }
  deriving (Eq, Ord, Show)

newtype AdjacencyRules = AdjacencyRules { getAdjacencyRules :: Map AdjacencyKey Bool }

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
