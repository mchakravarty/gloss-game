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

platformPos :: Point
platformPos = (-200, -150)

platformWidth, platformHeight :: Float
(_, (platformWidth, platformHeight)) = boundingBox (scale 0.5 0.5 platformSprite)


data World = World {creeperPos :: Point, creeperVelocity :: Float}

initialWorld :: World
initialWorld = World {creeperPos = (-200, 0), creeperVelocity = 0}

main = play (InWindow "Bouncy Creeper" (600, 400) (50, 50)) white 30 initialWorld draw handle [applyVelocity, applyGravity]
  where
    draw (World {creeperPos = (x, y)}) 
      = pictures [ translate x y (scale 0.5 0.5 creeperSprite)
                 , translate (fst platformPos) (snd platformPos) (scale 0.5 0.5 platformSprite)
                 ]
    
    handle (EventKey (Char 'a') Down _ _)            world = world {creeperPos = moveX (creeperPos world) (-10)}
    handle (EventKey (Char 'd') Down _ _)            world = world {creeperPos = moveX (creeperPos world) 10}
    handle (EventKey (SpecialKey KeySpace) Down _ _) world = world {creeperVelocity = 10}
    handle _event                         world            = world
    
    applyVelocity _ world
      = world {creeperPos = moveY (creeperPos world) (creeperVelocity world)}
    
    applyGravity _ world
      | onTheFloor world
        || onThePlatform world = world {creeperVelocity = creeperVelocity world * (-0.5)}
      | otherwise              = world {creeperVelocity = creeperVelocity world + gravitationalConstant}
    
    onTheFloor world = snd (creeperPos world) <= (-200) + creeperHeight / 2
    
    onThePlatform world = snd (creeperPos world) <= snd platformPos + creeperHeight / 2 + platformHeight / 2
                          && fst (creeperPos world) + creeperWidth / 2 > fst platformPos - platformWidth / 2
                          && fst (creeperPos world) - creeperWidth / 2 < fst platformPos + platformWidth / 2
    
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
                      else if y < snd platformPos + creeperHeight / 2 + platformHeight / 2 
                              && x + creeperWidth / 2 > fst platformPos - platformWidth / 2
                              && x - creeperWidth / 2 < fst platformPos + platformWidth / 2
                      then (x, snd platformPos + creeperHeight / 2 + platformHeight / 2)
                      else (x, y + offset)
