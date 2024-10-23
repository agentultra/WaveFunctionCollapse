{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad (unless)
import Linear (V4(..))
import Options.Applicative
import SDL
import qualified SDL.Image as Image
import System.FilePath

newtype Options
  = Options
  { image :: FilePath
  }
  deriving (Eq, Show)

parseOptions :: Parser Options
parseOptions = do
  image <- strOption (long "image")
  pure Options {..}

main :: IO ()
main = do
  options <- execParser $ info parseOptions fullDesc
  initializeAll
  Image.initialize [Image.InitPNG]
  window <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  img <- Image.loadTexture renderer options.image
  appLoop img renderer
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
  -- TODO (james): render the input surface
  copy renderer imgTexture Nothing Nothing
  present renderer
  unless qPressed (appLoop imgTexture renderer)
