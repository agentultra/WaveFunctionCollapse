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
import SDL.Raw.Types (PixelFormat (..))
import qualified SDL.Raw.Types as Raw

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

  img' <- toSurface inputTexture
  imgTexture <- createTextureFromSurface renderer img'
  appLoop imgTexture renderer

  -- let patternResults = WFC.patterns inputTexture 3
  --     inputWaveState = WFC.mkWaveState (15, 15) 100 patternResults
  --     outputWaveState = WFC.runWave inputWaveState WFC.collapseWave

  -- case WFC.mkOutputTexture patternResults outputWaveState of
  --   Left err -> putStrLn err >> destroyWindow window
  --   Right outputWFCTexture -> do
  --     print outputWFCTexture
  --     --img' <- toSurface outputWFCTexture
  --     img' <- toSurface inputTexture

  --     imgTexture <- createTextureFromSurface renderer img'
  --     appLoop imgTexture renderer
  --     destroyWindow window

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

-- 32-bit packed pixel format R G B A
fromSurface :: Surface -> IO (WFC.Texture CInt)
fromSurface img = withSurface img $ \imgSurface -> do
  (V2 imgW imgH) <- surfaceDimensions imgSurface
  print (imgW, imgH)
  unless (imgW == imgH) $ error "Input image must be square"
  pixelPtr <- surfacePixels imgSurface
  pixels <- sequence [getPixel imgSurface pixelPtr x y
                     | y <- [0..imgW - 1]
                     , x <- [0..imgH - 1]
                     ]
  pure $ WFC.textureFromList (fromIntegral imgW) pixels

toSurface :: WFC.Texture CInt -> IO Surface
toSurface wfcTexture = do
  let (_, (tw, th)) = Array.bounds wfcTexture.getTexture
  output <- createRGBSurface (fromIntegral <$> V2 tw th) RGBA8888
  bracket_ (lockSurface output) (unlockSurface output) $ do
    voidPixelPtr <- surfacePixels output
    let pixelPtr = castPtr @() @CInt voidPixelPtr
    sequence_ [setPixel output pixelPtr (fromIntegral x) (fromIntegral y) v | ((x, y), v) <- Array.assocs wfcTexture.getTexture]
  pure output

getPixel :: Surface -> Ptr () -> CInt -> CInt -> IO CInt
getPixel (Surface surfacePtr _) voidPixelPtr x y = do
  -- https://github.com/haskell-game/sdl2/issues/175
  rawSurface <- peek surfacePtr
  rawFormat <- peek $ Raw.surfaceFormat rawSurface
  let bpp = rawFormat.pixelFormatBytesPerPixel
      pixelPtr = castPtr @() @CInt voidPixelPtr
      pitch = Raw.surfaceW rawSurface * fromIntegral bpp
  peekByteOff pixelPtr (fromIntegral y * fromIntegral pitch + fromIntegral x * fromIntegral bpp)

setPixel :: Surface -> Ptr CInt -> CInt -> CInt -> CInt -> IO ()
setPixel (Surface surfacePtr _) pixelPtr x y pixelData = do
  rawSurface <- peek surfacePtr
  rawFormat <- peek $ Raw.surfaceFormat rawSurface
  let bpp = rawFormat.pixelFormatBytesPerPixel
      pitch = Raw.surfaceW rawSurface * fromIntegral bpp
  pokeByteOff pixelPtr (fromIntegral y * fromIntegral pitch + fromIntegral x * fromIntegral bpp) pixelData
