{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Pine.Internal.Renderer
  ( defaultApp
  , pine
  ) where

import Pine.Internal.Types

import SDL
import SDL.Image

import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad

type TextureCache = Map FilePath Texture

pine :: (Stateful s, Drawable s)
     => Text
     -> WindowConfig
     -> s
     -> IO ()
pine title windowConfig state_ = do
  initializeAll
  window <- createWindow title windowConfig
  renderer <- createRenderer window (-1) $ SDL.RendererConfig
    { SDL.rendererType = SDL.AcceleratedRenderer
    , SDL.rendererTargetTexture = False
    }
  let
    appLoop :: TextureCache -> IO ()
    appLoop cache = pollEvent >>= go cache (update state_)

    go :: (Stateful s, Drawable s) => TextureCache -> s -> Maybe Event -> IO ()
    go cache state mevent = do
      cache' <- drawCanvas cache $ draw state
      case mevent of
        Nothing -> pollEvent >>= go cache' (update state)
        Just ev -> case eventPayload ev of
          KeyboardEvent keyboardEvent
            |  keyboardEventKeyMotion keyboardEvent == Pressed &&
               keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
            -> pure ()
          _ -> pollEvent >>= go cache' (update state)

    drawCanvas :: TextureCache -> Canvas -> IO TextureCache
    drawCanvas cache canvas = do
      clear renderer
      cache' <- drawCanvas' cache canvas
      present renderer
      pure cache'

    drawCanvas' :: TextureCache -> Canvas -> IO TextureCache
    drawCanvas' cache canvas = do
      case canvas of
        SingleImage img -> drawImages cache [img]
        Images imgs -> drawImages cache imgs
        EmptyCanvas -> pure cache

    drawImages :: TextureCache -> [Image] -> IO TextureCache
    drawImages cache [] = pure cache
    drawImages cache (img:imgs) =
      case cache M.!? (imageSrc img) of
        Nothing -> do
          tex <- loadTexture renderer (imageSrc img)
          copy renderer tex Nothing Nothing
          drawImages (M.insert (imageSrc img) tex cache) imgs
        Just tex -> do
          copy renderer tex Nothing Nothing
          drawImages cache imgs

   in appLoop mempty

data DefaultState = Logo Image

instance Stateful DefaultState where
  initial = Logo $ newImage "src/Media/logo.png"
  update = id

instance Drawable DefaultState where
  draw (Logo img) = fromImage img


defaultApp :: IO ()
defaultApp = pine "Default App" defaultConfig (initial :: DefaultState)
  where
    defaultConfig = WindowConfig
      { windowBorder        = True
      , windowHighDPI       = False
      , windowInputGrabbed  = False
      , windowMode          = Windowed
      , windowOpenGL        = Nothing
      , windowPosition      = Wherever
      , windowResizable     = True
      , windowInitialSize   = V2 800 600
      , windowVisible       = True
      }

{-
  addEventWatch $ \ev ->
    case eventPayload ev of
      WindowSizeChangedEvent sizeChangeData ->
        putStrLn $ "eventWatch windowSizeChanged: " ++ show sizeChangeData
      KeyboardEvent kev ->
        putStrLn "key event"
      _ -> return ()
  appLoop mempty
    where
      appLoop :: TextureCache -> IO ()
      appLoop tc = pollEvent >>= go tc

      go :: TextureCache -> Maybe Event -> IO ()
      go tc = \case
        Nothing -> pollEvent >>= go tc
        Just ev -> case eventPayload ev of
          KeyboardEvent keyboardEvent
            |  keyboardEventKeyMotion keyboardEvent == Pressed &&
               keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
            -> return ()
          _ -> pollEvent >>= go tc



-}
