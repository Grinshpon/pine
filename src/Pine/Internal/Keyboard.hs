{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}


-- | Nothing here is complete. This module is extremely WIP
module Pine.Internal.Keyboard
  ( --keyboardState
  ) where

import qualified SDL
import Data.Map

data KeyStatus = Pressed | Released | Up | Down deriving (Eq)

keyboardState :: Map SDL.Keycode KeyStatus -> IO (Map SDL.Keycode KeyStatus)
keyboardState pState = do
  kbState <- SDL.getKeyboardState
  let nstate = diff pState (makeState kbState)
  pure nstate

diff :: Map SDL.Keycode KeyStatus -> Map SDL.Keycode KeyStatus -> Map SDL.Keycode KeyStatus
diff = undefined

makeState :: (SDL.Scancode -> Bool) -> Map SDL.Keycode KeyStatus
makeState f = undefined
