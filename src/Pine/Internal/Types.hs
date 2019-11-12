module Pine.Internal.Types where

import qualified SDL (Event)

import Data.Semigroup

-- DeltaTime Double | KeyPress | KeyRelease | KeyState deriving (Eq, Show) --ideas
-- | placeholder
data Event = DeltaTime Double | SDLEvent SDL.Event deriving (Eq, Show)


-- | Drawable class contains the draw function, which takes a type and converts it into a `Scene`
class Drawable d where
  draw :: d -> Scene

-- | Stateful class contains initial and update functions. Any objects in your game, including the overrall world, will update according to events that occur
class Stateful s where
  initial :: s
  update  :: Event -> s -> s

-- | An Image which is converted into a `Texture`
newtype Image = Image
  { imageSrc :: FilePath
  } deriving (Eq, Show) -- put in other info later (like dimensions, quads, etc)


-- | Create an image from a file
newImage :: FilePath -> Image
newImage = Image


data Media = MImage Image | MAudio | MText deriving (Eq, Show) -- WIP (TODO: replace instances of Image in Scene with Media

-- | A Scene can be empty, a single `Image`, or a group of `Image`s. (WIP: Later text and other stuff will be added)
data Scene = EmptyScene | SingleImage Image | Images [Image] deriving (Eq, Show)

instance Semigroup Scene where
  (<>) (SingleImage img1) (SingleImage img2) = Images [img1,img2]
  (<>) (Images imgs1) (Images imgs2) = Images $ imgs1 <> imgs2
  (<>) (SingleImage img) (Images imgs) = Images $ img:imgs
  (<>) (Images imgs) (SingleImage img) = Images $ imgs <> [img]
  (<>) EmptyScene c = c
  (<>) c EmptyScene = c

instance Monoid Scene where
  mempty = EmptyScene
  mappend = (<>)

-- | Convert a single `Image` into a `Scene`
fromImage :: Image -> Scene
fromImage = SingleImage
