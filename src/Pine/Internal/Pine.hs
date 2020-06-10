{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Pine.Internal.Pine
  ( pine
  , withDefaultConfig
  , with
  ) where

import Pine.Internal.Types

import qualified SDL
import qualified SDL.Image as SDLI
import Control.Concurrent.STM

import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad
import Control.Monad.State
import Data.Word (Word32)

type TextureCache = Map FilePath SDL.Texture

-- | This function initializes the window and takes an initial `Stateful` object that will be updated.
pine :: (Stateful s, Drawable s)
     => Text -- ^ Title
     -> SDL.WindowConfig -- ^ Window Configuration
     -> s -- ^ Initial state
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
      dcache <- updateEvents (fromIntegral time / 1000) [Load] state_ >>= \case
        Just loadState -> SDL.pollEvents >>= appStep time updateQueue cache loadState
        Nothing        -> pure cache
      _ <- SDL.removeTimer timer
      foldMap SDL.destroyTexture dcache
      SDL.destroyRenderer renderer
      SDL.destroyWindow window

    appStep :: (Stateful s, Drawable s) => Word32 -> TChan () -> TextureCache -> s -> [SDL.Event] -> IO TextureCache
    appStep time updateQueue cache state sdlEvents = do
      atomically $ readTChan updateQueue -- not ideal, events could get backed up
      time' <- SDL.ticks
      let dt = (fromIntegral (time' - time) :: Double) / 1000
      --print $ 1/dt
      cache' <- drawScene cache $ draw state
--      mousePos <- SDL.getModalMouseLocation >>= \case  --waiting for PR to be merged on sdl2
--        SDL.AbsoluteModalLocation (SDL.P pos) -> pos
--        SDL.RelativeModalLocation pos -> pos
      let pineEvents = eventState sdlEvents
      updateEvents dt (Step:pineEvents) state >>= \case
        Just newState -> SDL.pollEvents >>= appStep time' updateQueue cache' newState
        Nothing       -> pure cache'
      where
        eventState events =
          case events of
            [] -> []
            (ev:evs) ->
              [SDLEvent ev] <> case SDL.eventPayload ev of
                SDL.WindowClosedEvent _ -> [WindowClose] <> (eventState evs)
                SDL.KeyboardEvent keyboardEvent ->
                  case SDL.keyboardEventKeyMotion keyboardEvent of
                    SDL.Pressed  -> [KeyPressed  (SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent))] <> (eventState evs)
                    SDL.Released -> [KeyReleased (SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent))] <> (eventState evs)
                SDL.MouseMotionEvent mmeData -> [MouseMoved (SDL.mouseMotionEventRelMotion mmeData)] <> (eventState evs) -- maybe include new position as well
                _ -> (eventState evs)

    updateEvents :: (Stateful s, Drawable s) => DeltaTime -> [Event] -> s -> IO (Maybe s)
    updateEvents _  []     state = pure $ Just state
    updateEvents dt (e:es) state = do
      let (r, nState) = runState (update dt e) state
      case r of
        Cont          -> updateEvents dt es nState
        Log s         -> putStrLn s *> (updateEvents dt es nState)
        QuitWithLog s -> putStrLn s *> (pure $ Nothing)
        Quit          -> pure $ Nothing

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

withDefaultConfig = SDL.WindowConfig
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

with :: (MonadState s m) => (s -> (a,s)) -> m a
with = state
