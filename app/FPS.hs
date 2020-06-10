{-# LANGUAGE OverloadedStrings #-}

module Main where

import Pine
import qualified SDL
import Control.Monad.State

-- define the state of the game
data App = App
  { logo :: Image
  , frames :: (Int, Double)
  }

gameInit =
  App
    (newImage "src/Media/logo.png" Nothing (Just $ rect 200 200 400 400))
    (0,0)

inc :: DeltaTime -> PineState App
inc dt =
  with $ \state ->
    let (count,accum) = frames state
    in (Cont, state {frames = (count+1, accum+dt)})

instance Stateful App where
  update _ WindowClose               = quit
  update _ (KeyPressed SDL.KeycodeQ) = do
    appState <- get
    let (count,accum) = frames appState
    quitLog $ "Average FPS: " <> show (1 / (accum / fromIntegral count))
  update dt Step                     = inc dt
  update _ _                         = cont

instance Drawable App where
  draw = fromImage . logo

-- | This simply opens a window with the Pine logo displayed
main :: IO ()
main = pine "Pine" withDefaultConfig gameInit
