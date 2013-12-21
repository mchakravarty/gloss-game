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

    -- * Load sprites into pictures
  bmp, png, jpg,
  
    -- * More convenient game play
  play
) where

  -- standard libraries
import System.IO.Unsafe (unsafePerformIO)

  -- packages
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Data.Picture        hiding (Picture(..))
import Graphics.Gloss.Data.Picture        (Picture)             -- keep 'Picture' abstract
import Graphics.Gloss.Interface.Pure.Game (Event(..), Key(..), SpecialKey(..), MouseButton(..), KeyState(..))
import Graphics.Gloss.Juicy
import qualified Graphics.Gloss as Gloss


-- On-the-fly image loading
-- ------------------------

-- Turn a bitmap file into a picture.
--
-- NB: Define loaded pictures on the toplevel to avoid reloading.
--
bmp :: FilePath -> Picture
bmp fname = unsafePerformIO $ loadBMP fname

-- Turn a PNG file into a picture.
--
-- NB: Define loaded pictures on the toplevel to avoid reloading.
--
png :: FilePath -> Picture
png fname = maybe (text "PNG ERROR") id (unsafePerformIO $ loadJuicyPNG fname)

-- Turn a JPEG file into a picture.
--
-- NB: Define loaded pictures on the toplevel to avoid reloading.
--
jpg :: FilePath -> Picture
jpg fname = maybe (text "JPEG ERROR") id (unsafePerformIO $ loadJuicyJPG fname)


-- Extended play function
-- ----------------------

-- Play a game.
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
  = Gloss.play display bg fps world draw handler (perform steppers)
  where
    perform []                 _time world = world
    perform (stepper:steppers) time  world = perform steppers time (stepper time world)
