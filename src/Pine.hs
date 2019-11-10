
module Pine
  ( runDefault
  , pine
  , Drawable(..)
  , Stateful(..)
  , Image(..)
  , newImage
  , Canvas
  , fromImage
  , Event(..)
  ) where

import Pine.Internal

runDefault :: IO ()
runDefault = defaultApp
