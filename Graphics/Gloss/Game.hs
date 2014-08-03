-- |
-- Module      : Main
-- Copyright   : [2013] Manuel M T Chakravarty
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Portability : haskell2011

module Graphics.Gloss.Game (

    -- * Reexport some basic Gloss datatypes
  module Graphics.Gloss.Data.Color,
  module Graphics.Gloss.Data.Display,
  module Graphics.Gloss.Data.Picture,
  module Graphics.Gloss.Interface.Pure.Game,
  
    -- * Geometry
  Size, Rect,

    -- * Load sprites into pictures
  bmp, png, jpg,
  
    -- * Query pictures
  boundingBox,
  
    -- * More convenient game play
  play, playInScene,
  
    -- * Game scenes
  Animation, animation, noAnimation, animationPicture,
  Scene, picture, picturing, animating, translating, rotating, scaling, scenes,
  drawScene,
) where

  -- standard libraries
import Data.IORef
import Data.Maybe
import System.IO.Unsafe (unsafePerformIO)

  -- packages
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Data.Picture        hiding (Picture(..))
import Graphics.Gloss.Data.Picture        (Picture)             -- keep 'Picture' abstract
import Graphics.Gloss.Interface.Pure.Game (Event(..), Key(..), SpecialKey(..), MouseButton(..), KeyState(..))
import Graphics.Gloss.Juicy
import qualified Graphics.Gloss                   as G
import qualified Graphics.Gloss.Interface.IO.Game as G


-- Geometry
-- --------

type Size = (Float, Float)    -- ^width & height

type Rect = (Point, Size)     -- ^origin & extent, where the origin is at the centre


-- On-the-fly image loading
-- ------------------------

-- |Turn a bitmap file into a picture.
--
-- NB: Define loaded pictures on the toplevel to avoid reloading.
--
bmp :: FilePath -> Picture
bmp fname = unsafePerformIO $ loadBMP fname

-- |Turn a PNG file into a picture.
--
-- NB: Define loaded pictures on the toplevel to avoid reloading.
--
png :: FilePath -> Picture
png fname = maybe (text "PNG ERROR") id (unsafePerformIO $ loadJuicyPNG fname)

-- |Turn a JPEG file into a picture.
--
-- NB: Define loaded pictures on the toplevel to avoid reloading.
--
jpg :: FilePath -> Picture
jpg fname = maybe (text "JPEG ERROR") id (unsafePerformIO $ loadJuicyJPG fname)


-- Query pictures
-- --------------

-- |Determine the bounding box of a picture.
--
-- FIXME: Current implementation is incomplete!
--
boundingBox :: Picture -> Rect
boundingBox G.Blank                    = ((0, 0), (0, 0))
boundingBox (G.Polygon _)              = error "Graphics.Gloss.Game.boundingbox: Polygon not implemented yet"
boundingBox (G.Line _)                 = error "Graphics.Gloss.Game.boundingbox: Line not implemented yet"
boundingBox (G.Circle r)               = ((0, 0), (2 * r, 2 * r))
boundingBox (G.ThickCircle t r)        = ((0, 0), (2 * r + t, 2 * r + t))
boundingBox (G.Arc _ _ _)              = error "Graphics.Gloss.Game.boundingbox: Arc not implemented yet"
boundingBox (G.ThickArc _ _ _ _)       = error "Graphics.Gloss.Game.boundingbox: ThickArc not implemented yet"
boundingBox (G.Text _)                 = error "Graphics.Gloss.Game.boundingbox: Text not implemented yet"
boundingBox (G.Bitmap w h _ _)         = ((0, 0), (fromIntegral w, fromIntegral h))
boundingBox (G.Color _ p)              = boundingBox p
boundingBox (G.Translate dx dy p)      = let ((x, y), size) = boundingBox p in ((x + dx, y + dy), size)
boundingBox (G.Rotate _ang _p)         = error "Graphics.Gloss.Game.boundingbox: Rotate not implemented yet"
boundingBox (G.Scale xf yf p)          = let (origin, (w, h)) = boundingBox p in (origin, (w * xf, h * yf))
boundingBox (G.Pictures _ps)           = error "Graphics.Gloss.Game.boundingbox: Pictures not implemented yet"


-- Extended play function
-- ----------------------

-- |Play a game.
--
play :: Display                      -- ^Display mode
     -> Color                        -- ^Background color
     -> Int                          -- ^Number of simulation steps to take for each second of real time
     -> world                        -- ^The initial world state
     -> (world -> Picture)           -- ^A function to convert the world to a picture
     -> (Event -> world -> world)    -- ^A function to handle individual input events
     -> [Float -> world -> world]    -- ^Set of functions invoked once per iteration —
                                     --  first argument is the period of time (in seconds) needing to be advanced
     -> IO ()
play display bg fps world draw handler steppers
  = G.play display bg fps world draw handler (perform steppers)
  where
    perform []                 _time world = world
    perform (stepper:steppers) time  world = perform steppers time (stepper time world)

-- Global variable to keep track of the time since we started playing (there can only always be one game anyway).
--
currentTime :: IORef Float
{-# NOINLINE currentTime #-}
currentTime = unsafePerformIO $ newIORef 0

-- |Play a game in a scene.
--
playInScene :: Display                               -- ^Display mode
            -> Color                                 -- ^Background color
            -> Int                                   -- ^Number of simulation steps to take for each second of real time
            -> world                                 -- ^The initial world state
            -> Scene world                           -- ^A scene parameterised by the world
            -> (Float -> Event -> world -> world)    -- ^A function to handle individual input events
                                                     --  * first argument is the absolute time (in seconds)
            -> [Float -> Float -> world -> world]    -- ^Set of functions invoked once per iteration —
                                                     --  * first argument is the absolute time (in seconds)
                                                     --  * second argument is the period of time needing to be advanced
            -> IO ()
playInScene display bg fps world scene handler steppers
  = G.playIO display bg fps world drawSceneNow performHandler (advanceTimeAndPerform steppers)
  where
    drawSceneNow world
      = do
        { now <- readIORef currentTime
        ; return $ drawScene scene now world
        }
        
    performHandler event world 
      = do
        { now <- readIORef currentTime
        ; return $ handler now event world
        }

    advanceTimeAndPerform steppers deltaT world
      = do 
        { now <- readIORef currentTime
        ; let future = now + deltaT
        ; writeIORef currentTime future
        ; perform steppers future deltaT world
        }

    perform []                 _now _deltaT world = return world
    perform (stepper:steppers) now  deltaT  world = perform steppers now deltaT (stepper now deltaT world)


-- Scenes are parameterised pictures
-- ---------------------------------

-- |An abstract representation of an animation.
--
data Animation = Animation [Picture] Float Float

-- |Construct a new animation with a list of pictures for the animation, the time between animation frames, and a given
-- (absolute) start time.
--
animation :: [Picture] -> Float -> Float -> Animation
animation = Animation

-- |An empty animation.
--
noAnimation :: Animation
noAnimation = animation [] 1 0

animationPicture :: Animation -> Float -> Maybe Picture
animationPicture (Animation pics delay start) time
  | start > time     = Nothing
  | i >= length pics = Nothing
  | otherwise        = Just $ pics !! i
  where
    i = round ((time - start) / delay)

-- |A scene describes the rendering of a world state — i.e., which picture should be draw depending on the current time
-- and of the state of the world.
--
data Scene world
  = Picturing   (Float -> world -> Picture)
  | Translating (         world -> Point)          (Scene world)
  | Rotating    (         world -> Float)          (Scene world)
  | Scaling     (         world -> (Float, Float)) (Scene world)
  | Scenes                                         [Scene world]

-- |Turn a static picture into a scene.
--
picture :: Picture -> Scene world
picture p = picturing (const p)

-- |Turn a world-dependent picture into a scene.
--
picturing :: (world -> Picture) -> Scene world
picturing worldToPic = Picturing (const worldToPic)

-- |Animate a world-dependent animation. The default picture is displayed while no animation is running.
--
animating :: (world -> Animation) -> Picture -> Scene world
animating anim defaultPic
  = Picturing (\currentTime world -> fromMaybe defaultPic $ animationPicture (anim world) currentTime)

-- |Move a scene in dependences on a world-dependent location.
--
translating :: (world -> Point) -> Scene world -> Scene world
translating = Translating

-- |Rotate a scene in dependences on a world-dependent angle.
--
rotating :: (world -> Float) -> Scene world -> Scene world
rotating = Rotating

-- |Scale a scene in dependences on world-dependent scaling factors.
--
scaling :: (world -> (Float, Float)) -> Scene world -> Scene world
scaling = Scaling

-- |Compose a scene from a list of scenes.
--
scenes :: [Scene world] -> Scene world
scenes = Scenes

-- |Render a scene on the basis of time since playing started and the specific world state.
--
drawScene :: Scene world -> Float -> world -> Picture
drawScene scene time world = drawS scene
  where
    drawS (Picturing draw)             = draw time world
    drawS (Translating movement scene) = let (x, y) = movement world in translate x y (drawS scene)
    drawS (Rotating rotation scene)    = rotate (rotation world) (drawS scene)
    drawS (Scaling scaling scene)      = let (xf, yf) = scaling world in scale xf yf (drawS scene)
    drawS (Scenes scenes)              = pictures $ map drawS scenes


-- -- Game objects
-- -- ------------
-- 
-- data Object objClass = 