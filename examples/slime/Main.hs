-- Very simple example of moving a character and shooting a projectile.

import Data.Maybe
import Graphics.Gloss.Game

slimeSprite :: Picture
slimeSprite = bmp "Slime.bmp"

slimeBallSprite :: Picture
slimeBallSprite = bmp "SlimeBall.bmp"

data World = World {slimePos :: Point, ballPos :: Maybe Point }

initialWorld :: World
initialWorld = World {slimePos = (0, 0), ballPos = Nothing}

main = play (InWindow "Slime" (600, 400) (50, 50)) white 30 initialWorld draw handle [boxInSlime, moveBall]
  where
    draw (World {slimePos = (x, y), ballPos = ballPos}) 
      = pictures [ translate x y (scale 0.5 0.5 slimeSprite)
                 , maybeDrawBall ballPos
                 ]
    
    maybeDrawBall Nothing       = blank
    maybeDrawBall (Just (x, y)) = translate x y (scale 0.5 0.5 slimeBallSprite)
    
    handle (EventKey (Char 'a') Down _ _)            world = world {slimePos = moveX (slimePos world) (-10)}
    handle (EventKey (Char 'd') Down _ _)            world = world {slimePos = moveX (slimePos world) 10}
    handle (EventKey (Char 's') Down _ _)            world = world {slimePos = moveY (slimePos world) (-10)}
    handle (EventKey (Char 'w') Down _ _)            world = world {slimePos = moveY (slimePos world) 10}
    handle (EventKey (SpecialKey KeySpace) Down _ _) world = if isNothing (ballPos world)
                                                             then world {ballPos = Just (slimePos world)}
                                                             else world
    handle _event                         world            = world
    
    boxInSlime _ world = let (x, y) = slimePos world
                         in 
                         world { slimePos = ((x `max` (-300)) `min` 300, 
                                             (y `max` (-200)) `min` 200) }
    
    moveBall _ world@(World {ballPos = Nothing})     = world
    moveBall _ world@(World {ballPos = Just (x, y)}) = if x > 300
                                                       then world {ballPos = Nothing}
                                                       else world {ballPos = Just (x + 8, y)}

moveX :: Point -> Float -> Point
moveX (x, y) offset = (x + offset, y)

moveY :: Point -> Float -> Point
moveY (x, y) offset = (x, y + offset)
