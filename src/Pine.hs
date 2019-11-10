module Pine
  ( runDefault
  , pine
  , Drawable
  , draw
  , Stateful
  , initial
  , update
  , Image
  , imageSrc
  , newImage
  , Canvas
  , fromImage
  ) where

import Pine.Internal

runDefault :: IO ()
runDefault = defaultApp
