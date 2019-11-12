
-- | Everything you need is reixported by Pine, so you do not need to import any of the internal modules.
module Pine
  ( runDefault
  , pine
  , Drawable(..)
  , Stateful(..)
  , Image(..)
  , newImage
  , rect
  , Scene
  , fromImage
  , Event(..)
  ) where

import Pine.Internal


-- | Run the default program, which displays the Pine logo
runDefault :: IO ()
runDefault = defaultApp
