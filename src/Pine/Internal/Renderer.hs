{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Pine.Internal.Renderer
  ( defaultApp
  , pine
  ) where

import Pine.Internal.Types

import qualified SDL
import qualified SDL.Image as SDLI
import Control.Concurrent.STM

import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad
import Data.Word (Word32)

type TextureCache = Map FilePath SDL.Texture

pine :: (Stateful s, Drawable s)
     => Text
     -> SDL.WindowConfig
     -> s
     -> IO ()
pine title windowConfig state_ = do
  SDL.initializeAll
  window <- SDL.createWindow title windowConfig
  renderer <- SDL.createRenderer window (-1) $ SDL.RendererConfig
    { SDL.rendererType = SDL.AcceleratedRenderer
    , SDL.rendererTargetTexture = False
    }
  let
    appLoop :: TextureCache -> IO ()
    appLoop cache = do
      updateQueue <- newTChanIO
      timer <- SDL.addTimer 16 (fpsTimer updateQueue)
      time <- SDL.ticks
      SDL.pollEvent >>= go time updateQueue cache state_
      _ <- SDL.removeTimer timer
      pure ()

    go :: (Stateful s, Drawable s) => Word32 -> TChan () -> TextureCache -> s -> Maybe SDL.Event -> IO ()
    go time updateQueue cache state mevent = do
      time' <- SDL.ticks
      let dt = fromIntegral (time' - time) :: Double
      atomically $ readTChan updateQueue -- not ideal, events could get backed up
      cache' <- drawCanvas cache $ draw state
      case mevent of
        Nothing -> SDL.pollEvent >>= go time' updateQueue cache' (update (DeltaTime dt) state)
        Just ev -> case SDL.eventPayload ev of
          SDL.WindowClosedEvent _ -> pure ()
          _ -> SDL.pollEvent >>= go time' updateQueue cache' (update (SDLEvent ev) state)

    fpsTimer :: TChan () -> Word32 -> IO SDL.RetriggerTimer
    fpsTimer updateQueue _ = do
      atomically $ writeTChan updateQueue ()
      pure $ SDL.Reschedule 16

    drawCanvas :: TextureCache -> Canvas -> IO TextureCache
    drawCanvas cache canvas = do
      SDL.clear renderer
      cache' <- drawCanvas' cache canvas
      SDL.present renderer
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
          tex <- SDLI.loadTexture renderer (imageSrc img)
          SDL.copy renderer tex Nothing Nothing
          drawImages (M.insert (imageSrc img) tex cache) imgs
        Just tex -> do
          SDL.copy renderer tex Nothing Nothing
          drawImages cache imgs

   in appLoop mempty

data DefaultState = Logo Image

instance Stateful DefaultState where
  initial = Logo $ newImage "src/Media/logo.png"
  update = const id

instance Drawable DefaultState where
  draw (Logo img) = fromImage img

defaultApp :: IO ()
defaultApp = pine "Pine" defaultConfig (initial :: DefaultState)
  where
    defaultConfig = SDL.WindowConfig
      { SDL.windowBorder        = True
      , SDL.windowHighDPI       = False
      , SDL.windowInputGrabbed  = False
      , SDL.windowMode          = SDL.Windowed
      , SDL.windowOpenGL        = Nothing
      , SDL.windowPosition      = SDL.Wherever
      , SDL.windowResizable     = True
      , SDL.windowInitialSize   = SDL.V2 800 800
      , SDL.windowVisible       = True
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
