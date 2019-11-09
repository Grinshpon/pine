{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Pine.Internal.Keyboard
  ( keyboardState
  ) where

import SDL
import Data.Map

data KeyStatus = Pressed | Released | Up | Down deriving (Eq)

keyboardState :: Map Keycode KeyStatus -> IO (Map Keycode KeyStatus)
keyboardState pState = do
  kbState <- getKeyboardState
  let nstate = diff pState (makeState kbState)
  pure nstate

diff :: Map Keycode KeyStatus -> Map Keycode KeyStatus -> Map Keycode KeyStatus
diff = undefined

makeState :: (Scancode -> Bool) -> Map Keycode KeyStatus
makeState f = undefined
