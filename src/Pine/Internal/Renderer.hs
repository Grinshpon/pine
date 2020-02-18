{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Pine.Internal.Renderer
  ( pine
  , withDefaultConfig
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

-- | This function initializes the window and takes an initial `Stateful` object that will be updated.
pine :: (Stateful s, Drawable s)
     => Text -- ^ Title
     -> GameState s -- ^ Window configuration and Initial state
     -> IO ()
pine title gs@GameState{..} = do
  SDL.initializeAll
  window <- SDL.createWindow title gameWindow
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
      SDL.pollEvent >>= go time updateQueue cache gs
      _ <- SDL.removeTimer timer
      pure ()

    go :: (Stateful s, Drawable s) => Word32 -> TChan () -> TextureCache -> GameState s -> Maybe SDL.Event -> IO ()
    go time updateQueue cache state@GameState{..} mevent = do
      atomically $ readTChan updateQueue -- not ideal, events could get backed up
      time' <- SDL.ticks
      let dt = (fromIntegral (time' - time) :: Double) / 1000
      --print $ 1/dt
      cache' <- drawScene cache $ draw gameState
      case mevent of
        Nothing -> SDL.pollEvent >>= go time' updateQueue cache' (update (DeltaTime dt) state)
        Just ev -> case SDL.eventPayload ev of
          SDL.WindowClosedEvent _ -> pure () -- replace with go ... CloseEvent
          _ -> SDL.pollEvent >>= go time' updateQueue cache' (update (SDLEvent ev) $ update (DeltaTime dt) state)

    fpsTimer :: TChan () -> Word32 -> IO SDL.RetriggerTimer
    fpsTimer updateQueue _ = do
      atomically $ writeTChan updateQueue ()
      pure $ SDL.Reschedule 16

    drawScene :: TextureCache -> Scene -> IO TextureCache
    drawScene cache canvas = do
      SDL.clear renderer
      cache' <- drawScene' cache canvas
      SDL.present renderer
      pure cache'

    drawScene' :: TextureCache -> Scene -> IO TextureCache
    drawScene' cache canvas = do
      case canvas of
        SingleScene m -> loadMedia cache [m]
        MultiScene ms -> loadMedia cache ms
        EmptyScene    -> pure cache

    loadMedia :: TextureCache -> [Media] -> IO TextureCache
    loadMedia cache [] = pure cache
    loadMedia cache (m:imgs) =
      case m of
        MImage img ->
          case cache M.!? (imageSrc img) of
            Nothing -> do
              tex <- SDLI.loadTexture renderer (imageSrc img)
              SDL.copy renderer tex Nothing Nothing
              loadMedia (M.insert (imageSrc img) tex cache) imgs
            Just tex -> do
              SDL.copy renderer tex (imageQuad img) (imageRect img)
              loadMedia cache imgs
        _ -> loadMedia cache imgs

   in appLoop mempty

withDefaultConfig :: (Stateful s, Drawable s) => s -> GameState s
withDefaultConfig s = GameState defaultConfig s False
  where
    defaultConfig = SDL.WindowConfig
      { SDL.windowBorder        = True
      , SDL.windowHighDPI       = False
      , SDL.windowInputGrabbed  = False
      , SDL.windowMode          = SDL.Windowed
      , SDL.windowGraphicsContext = SDL.NoGraphicsContext
      , SDL.windowPosition      = SDL.Wherever
      , SDL.windowResizable     = True
      , SDL.windowInitialSize   = SDL.V2 800 800
      , SDL.windowVisible       = True
      }
