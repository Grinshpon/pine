module Pine.Internal.Types where

--import SDL

import Data.Semigroup

data Event = DeltaTime Double | KeyPress | KeyRelease | KeyState deriving (Eq, Show) --placeholder

class Drawable d where
  draw :: d -> Canvas --Foldable f => d -> f Image

class Stateful s where
  initial :: s
  update  :: Event -> s -> s -- Event -> s -> s

newtype Image = Image
  { imageSrc :: FilePath
  } deriving (Eq, Show) -- put in other info later (like dimensions, quads, etc)

newImage :: FilePath -> Image
newImage = Image

data Canvas = EmptyCanvas | SingleImage Image | Images [Image] deriving (Eq, Show)

instance Semigroup Canvas where
  (<>) (SingleImage img1) (SingleImage img2) = Images [img1,img2]
  (<>) (Images imgs1) (Images imgs2) = Images $ imgs1 <> imgs2
  (<>) (SingleImage img) (Images imgs) = Images $ img:imgs
  (<>) (Images imgs) (SingleImage img) = Images $ imgs <> [img]
  (<>) EmptyCanvas c = c
  (<>) c EmptyCanvas = c

instance Monoid Canvas where
  mempty = EmptyCanvas
  mappend = (<>)

fromImage :: Image -> Canvas
fromImage = SingleImage
