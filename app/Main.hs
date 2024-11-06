{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import qualified Algorithm.WaveFunctionCollapse as WFC
import Control.Exception
import Control.Monad (unless, when)
import Data.Word
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
  inputTexture <- withSurface img $ \imgSurface -> do
    (V2 imgW imgH) <- surfaceDimensions imgSurface
    when (imgW == imgH) $ error "Input image must be square"
    pixelPtr <- surfacePixels imgSurface
    pixels <- sequence [getPixel pixelPtr x | x <- [0..(imgW*imgH) - 1]]
    pure $ WFC.textureFromList (fromIntegral imgW) pixels

  print inputTexture
  imgTexture <- createTextureFromSurface renderer img
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

getPixel :: Ptr () -> CInt -> IO CUInt
getPixel voidPixelPtr ix = do
  let pixelPtr = castPtr @() @CUInt voidPixelPtr
  peekElemOff pixelPtr (fromIntegral ix)
