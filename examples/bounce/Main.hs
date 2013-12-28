-- Very simple example of bouncing and jumping character.

import Data.Maybe
import Graphics.Gloss.Game

creeperSprite :: Picture
creeperSprite = bmp "Creeper.bmp"

platformSprite :: Picture
platformSprite = bmp "Platform.bmp"

gravitationalConstant :: Float
gravitationalConstant = -0.5

creeperWidth, creeperHeight :: Float
(_, (creeperWidth, creeperHeight)) = boundingBox (scale 0.5 0.5 creeperSprite)

data World = World {creeperPos :: Point, creeperVelocity :: Float, platformPos :: Point }

initialWorld :: World
initialWorld = World {creeperPos = (-200, 0), creeperVelocity = 0, platformPos = (-200, -150)}

main = play (InWindow "Bouncy Creeper" (600, 400) (50, 50)) white 30 initialWorld draw handle [applyGravity]
  where
    draw (World {creeperPos = (x, y), platformPos = (platformX, platformY)}) 
      = pictures [ translate x y (scale 0.5 0.5 creeperSprite)
                 , translate platformX platformY (scale 0.5 0.5 platformSprite)
                 ]
    
    handle (EventKey (Char 'a') Down _ _)            world = world {creeperPos = moveX (creeperPos world) (-10)}
    handle (EventKey (Char 'd') Down _ _)            world = world {creeperPos = moveX (creeperPos world) 10}
    handle (EventKey (SpecialKey KeySpace) Down _ _) world = world {creeperVelocity = 10}
    handle _event                         world            = world
    
    applyGravity _ world@(World {creeperVelocity = velocity}) 
      = world { creeperPos      = moveY (creeperPos world) velocity
              , creeperVelocity = velocity + gravitationalConstant}

moveX :: Point -> Float -> Point
moveX (x, y) offset = if x + offset > 300 - creeperWidth / 2
                      then (300 - creeperWidth / 2, y)
                      else if x + offset < (-300) + creeperWidth / 2
                      then ((-300) + creeperWidth / 2, y)
                      else (x + offset, y)

moveY :: Point -> Float -> Point
moveY (x, y) offset = if y + offset > 200 - creeperHeight / 2
                      then (x, 200 - creeperHeight / 2)
                      else if y + offset < (-200) + creeperHeight / 2
                      then (x, (-200) + creeperHeight / 2)
                      else (x, y + offset)
