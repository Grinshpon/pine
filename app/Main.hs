{-# LANGUAGE OverloadedStrings #-}

module Main where

import Pine
import qualified SDL

-- define the state of the game
data DefaultState = Logo Image

defaultInitial = Logo $ newImage "src/Media/logo.png" Nothing (Just $ rect 200 200 400 400)

instance Stateful DefaultState where
  update _ Load                      = return $ Log "Hello, Pine!"
  update _ WindowClose               = return Quit
  update _ (KeyPressed SDL.KeycodeQ) = return $ QuitWithLog "Goodbye, Pine!"
  update _ _                         = return Cont

instance Drawable DefaultState where
  draw (Logo img) = fromImage img

-- | This simply opens a window with the Pine logo displayed
main :: IO ()
main = pine "Pine" withDefaultConfig defaultInitial
