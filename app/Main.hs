{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import qualified Algorithm.WaveFunctionCollapse as WFC
import Control.Exception
import Control.Monad (unless)
import qualified Data.Array as Array
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Options.Applicative
import SDL
import qualified SDL.Image as Image

newtype Options
  = Options
  { image :: FilePath
  }
  deriving (Eq, Show)

parseOptions :: Parser Options
parseOptions = do
  image <- strOption (long "image")
  pure Options {..}

withSurface :: Surface -> (Surface -> IO a) -> IO a
withSurface surface = bracket (lockSurface surface >> pure surface) unlockSurface

main :: IO ()
main = do
  options <- execParser $ info parseOptions fullDesc
  initializeAll
  Image.initialize [Image.InitPNG]
  window <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  img <- Image.load options.image

  inputTexture <- fromSurface img

  print inputTexture

  let patternResults = WFC.patterns inputTexture 3
      inputWaveState = WFC.mkWaveState (15, 15) 100 patternResults
      outputWaveState = WFC.runWave inputWaveState WFC.collapseWave

  case WFC.mkOutputTexture patternResults outputWaveState of
    Left err -> putStrLn err >> destroyWindow window
    Right outputWFCTexture -> do
      print outputWFCTexture
      img' <- toSurface outputWFCTexture

      imgTexture <- createTextureFromSurface renderer img'
      appLoop imgTexture renderer
      destroyWindow window

appLoop :: Texture -> Renderer -> IO ()
appLoop imgTexture renderer = do
  events <- pollEvents
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events
  rendererDrawColor renderer $= V4 0 0 255 255
  clear renderer
  copy renderer imgTexture Nothing Nothing
  present renderer
  unless qPressed (appLoop imgTexture renderer)

fromSurface :: Surface -> IO (WFC.Texture CUInt)
fromSurface img = withSurface img $ \imgSurface -> do
  (V2 imgW imgH) <- surfaceDimensions imgSurface
  print (imgW, imgH)
  unless (imgW == imgH) $ error "Input image must be square"
  pixelPtr <- surfacePixels imgSurface
  pixels <- sequence [getPixel pixelPtr x | x <- [0..(imgW*imgH) - 1]]
  pure $ WFC.textureFromList (fromIntegral imgW) pixels

toSurface :: WFC.Texture CUInt -> IO Surface
toSurface wfcTexture = do
  let (_, (tw, th)) = Array.bounds wfcTexture.getTexture
      twI = fromIntegral tw - 1
  output <- createRGBSurface (fromIntegral <$> V2 tw th) RGBA8888
  bracket_ (lockSurface output) (unlockSurface output) $ do
    voidPixelPtr <- surfacePixels output
    let pixelPtr = castPtr @() @CUInt voidPixelPtr
    sequence_ [setPixel pixelPtr ((fromIntegral y * twI) + fromIntegral x) v | ((x, y), v) <- Array.assocs wfcTexture.getTexture]
  pure output

getPixel :: Ptr () -> CInt -> IO CUInt
getPixel voidPixelPtr ix = do
  let pixelPtr = castPtr @() @CUInt voidPixelPtr
  peekElemOff pixelPtr (fromIntegral ix)

setPixel :: Ptr CUInt -> Int -> CUInt -> IO ()
setPixel = pokeByteOff
