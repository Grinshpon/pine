{-# LANGUAGE OverloadedStrings #-}

module Main where

import Pine

-- define the state of the game
data DefaultState = Logo Image

defaultInitial = Logo $ newImage "src/Media/logo.png" Nothing (Just $ rect 200 200 400 400)

instance Stateful DefaultState where
  update = const id
  quit   = const True

instance Drawable DefaultState where
  draw (Logo img) = fromImage img

-- | This simply opens a window with the Pine logo displayed
defaultApp :: IO ()
defaultApp = pine "Pine" withDefaultConfig defaultInitial

main :: IO ()
main = defaultApp
