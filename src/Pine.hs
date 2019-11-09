module Pine
  ( run
  , Pine.Internal.pine
  ) where

import Pine.Internal

run :: IO ()
run = defaultApp
