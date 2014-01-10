-- Very simple example of bouncing and jumping character.

import Data.Maybe
import Graphics.Gloss.Game

slimeSprite :: Picture
slimeSprite = bmp "Slime.bmp"

platformSprite :: Picture
platformSprite = bmp "Platform.bmp"

gravitationalConstant :: Float
gravitationalConstant = -0.5

slimeWidth, slimeHeight :: Float
(_, (slimeWidth, slimeHeight)) = boundingBox (scale 0.5 0.5 slimeSprite)

platformPos :: Point
platformPos = (-200, -150)

platformWidth, platformHeight :: Float
(_, (platformWidth, platformHeight)) = boundingBox (scale 0.5 0.5 platformSprite)


data World 
  = World
    { pressedKeys   :: [Char]
    , slimePos      :: Point
    , slimeVelocity :: Float
    }

initialWorld :: World
initialWorld 
  = World 
    { pressedKeys   = []
    , slimePos      = (-200, 0)
    , slimeVelocity = 0
    }

main
  = play (InWindow "Bouncy Slime" (600, 400) (50, 50)) white 30 initialWorld draw handle
    [applyMovement, applyVelocity, applyGravity]
  where
    draw (World {slimePos = (x, y)}) 
      = pictures [ translate x y (scale 0.5 0.5 slimeSprite)
                 , translate (fst platformPos) (snd platformPos) (scale 0.5 0.5 platformSprite)
                 ]
    
    handle (EventKey (Char c) Down _ _)              world = world {pressedKeys = c : pressedKeys world}
    handle (EventKey (Char c) Up   _ _)              world = world {pressedKeys = filter (/= c) (pressedKeys world)}
    handle (EventKey (SpecialKey KeySpace) Down _ _) world = world {slimeVelocity = 10}
    handle _event                         world            = world

    applyMovement _ world | 'a' `elem` pressedKeys world = world {slimePos = moveX (slimePos world) (-10)}
                          | 'd' `elem` pressedKeys world = world {slimePos = moveX (slimePos world) 10}
                          | otherwise                    = world                
    
    applyVelocity _ world
      = world {slimePos = moveY (slimePos world) (slimeVelocity world)}
    
    applyGravity _ world
      | onTheFloor world
        || onThePlatform world = world {slimeVelocity = slimeVelocity world * (-0.5)}
      | otherwise              = world {slimeVelocity = slimeVelocity world + gravitationalConstant}
    
    onTheFloor world = snd (slimePos world) <= (-200) + slimeHeight / 2
    
    onThePlatform world = snd (slimePos world) <= snd platformPos + slimeHeight / 2 + platformHeight / 2
                          && fst (slimePos world) + slimeWidth / 2 > fst platformPos - platformWidth / 2
                          && fst (slimePos world) - slimeWidth / 2 < fst platformPos + platformWidth / 2
    
moveX :: Point -> Float -> Point
moveX (x, y) offset 
  | x + offset > 300 - slimeWidth / 2    = (300 - slimeWidth / 2, y)
  | x + offset < (-300) + slimeWidth / 2 = ((-300) + slimeWidth / 2, y)
  | otherwise                            = (x + offset, y)

moveY :: Point -> Float -> Point
moveY (x, y) offset 
  | y + offset > 200 - slimeHeight / 2 
  = (x, 200 - slimeHeight / 2)
  | y + offset < (-200) + slimeHeight / 2 
  = (x, (-200) + slimeHeight / 2)
  | y + offset < snd platformPos + slimeHeight / 2 + platformHeight / 2 
    && x + slimeWidth / 2 > fst platformPos - platformWidth / 2
    && x - slimeWidth / 2 < fst platformPos + platformWidth / 2
  = (x, snd platformPos + slimeHeight / 2 + platformHeight / 2)
  | otherwise
  = (x, y + offset)
