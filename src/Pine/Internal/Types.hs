module Pine.Internal.Types where

import qualified SDL (Event, Rectangle(..))
import qualified SDL.Vect as SDLV
import Foreign.C.Types (CInt)

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
data Image = Image
  { imageSrc  :: FilePath
  , imageQuad :: Maybe (SDL.Rectangle CInt)
  , imageSize :: Maybe (SDL.Rectangle CInt)
  } deriving (Eq, Show) -- put in other info later (like dimensions, quads, etc)

-- | Construct a rectangle
rect :: ()
     => CInt -- ^ x
     -> CInt -- ^ y
     -> CInt -- ^ w
     -> CInt -- ^ h
     -> SDL.Rectangle CInt
rect x y w h = SDL.Rectangle (SDLV.P $ SDLV.V2 x y) (SDLV.V2 w h)


-- | Create an image from a file
newImage :: ()
         => FilePath -- ^ The source of the image file
         -> Maybe (SDL.Rectangle CInt) -- ^ A quad or Nothing for the whole image
         -> Maybe (SDL.Rectangle CInt) -> Image -- ^ The rendering target: The location of the image on the window and its size, or Nothing to take up the whole window.
newImage = Image


newtype Audio = Audio -- WIP
  { audiosrc :: FilePath
  } deriving (Eq, Show)

data AudioState = Playing Audio | Stopped Audio deriving (Eq, Show) --WIP

data Playback = Playback Audio | Continue Audio | Stop Audio deriving (Eq, Show) -- WIP

data Media = MImage Image | MAudio | MText deriving (Eq, Show) -- WIP (TODO: replace instances of Image in Scene with Media

-- | A Scene can be empty, a single `Image`, or a group of `Image`s. (WIP: Later text and other stuff will be added)
data Scene = EmptyScene | SingleScene Media | MultiScene [Media] deriving (Eq, Show)

instance Semigroup Scene where
  (<>) (SingleScene img1) (SingleScene img2) = MultiScene [img1,img2]
  (<>) (MultiScene imgs1) (MultiScene imgs2) = MultiScene $ imgs1 <> imgs2
  (<>) (SingleScene img) (MultiScene imgs) = MultiScene $ img:imgs
  (<>) (MultiScene imgs) (SingleScene img) = MultiScene $ imgs <> [img]
  (<>) EmptyScene c = c
  (<>) c EmptyScene = c

instance Monoid Scene where
  mempty = EmptyScene
  mappend = (<>)

-- | Convert a single `Image` into a `Scene`
fromImage :: Image -> Scene
fromImage = SingleScene . MImage
