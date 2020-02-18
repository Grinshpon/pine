{-# LANGUAGE OverloadedStrings #-}

module Main where

import Pine

-- create Stateful and Drawable instances for GameState
instance (Stateful s) => Stateful (GameState s) where
  update event state = state {gameState = update event $ gameState state}

instance (Drawable d) => Drawable (GameState d) where
  draw = draw . gameState
data DefaultState = Logo Image

-- define the state of the game
defaultInitial = Logo $ newImage "src/Media/logo.png" Nothing (Just $ rect 200 200 400 400)

instance Stateful DefaultState where
  update = const id

instance Drawable DefaultState where
  draw (Logo img) = fromImage img

-- | This simply opens a window with the Pine logo displayed
defaultApp :: IO ()
defaultApp = pine "Pine" $ withDefaultConfig defaultInitial

main :: IO ()
main = defaultApp
