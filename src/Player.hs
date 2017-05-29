module Player where

import Model
import Physics
import Config
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Vector

initPlayerParts :: [PlayerPart]
initPlayerParts = [PlayerPart { playerMass = startMass
                              , playerRadius = radiusfromMass startMass
                              , playerSpeed = startSpeed
                              , playerPos = (0, 0)
                              , playerDirection = 0.0
                              , timeForSpeed = 0.0 }]

initPlayerParts1 :: [PlayerPart]
initPlayerParts1 = [PlayerPart { playerMass = startMass
                              , playerRadius = radiusfromMass startMass
                              , playerSpeed = startSpeed
                              , playerPos = (100, 100)
                              , playerDirection = 0.0
                              , timeForSpeed = 0.0 }]

initPlayer :: Player
initPlayer = Player 
  { playerID = 1
  , playerColor = green
  , playerType = Handle
  , playerTarget = (0, 0)
  , playerCenterMass = (0, 0)
  , playerParts = initPlayerParts
  , timeFromSplit = 0.0
  }

initPlayer1 :: Player
initPlayer1 = Player 
  { playerID = 2
  , playerColor = yellow
  , playerType = Bot Dummy
  , playerTarget = (100, 100)
  , playerCenterMass = (100, 100)
  , playerParts = initPlayerParts1
  , timeFromSplit = 0.0
  }

drawPlayerPart :: PlayerPart -> Picture
drawPlayerPart p = uncurry translate (playerPos p) (circleSolid (playerRadius p))

drawPlayer :: Player -> Picture
drawPlayer p = color (playerColor p) (pictures (map drawPlayerPart (playerParts p)))

movePlayer :: Point -> Player -> Player
movePlayer m p = p
  { playerTarget = newTarget
  , playerParts = map (\x -> x {playerDirection = argV (newTarget - (playerPos x))}) (playerParts p)  
  }
  where
    newTarget = checkBoards m

splitPlayerPart :: PlayerPart -> [PlayerPart]
splitPlayerPart p = [ p{ playerMass = newMass, playerRadius = newRadius, playerSpeed = newSpeed}
                    , p{ playerMass = newMass, playerRadius = newRadius, timeForSpeed = 1, playerSpeed = newSpeed}]
    where
      newMass = (playerMass p) / 2
      newRadius = radiusfromMass newMass
      newSpeed = speedFromMass newMass

splitPlayer :: Player -> Player
splitPlayer p = p 
  { playerParts = foldl (\x y -> (x ++ splitPlayerPart y)) [] (playerParts p)
  , timeFromSplit = 10
  } 

updatePlayerParts :: Float -> Point -> PlayerPart -> PlayerPart
updatePlayerParts dt m p  = p { playerPos = (playerPos p) + motion, timeForSpeed = max 0 $ (timeForSpeed p) - dt}
  where
    playerSpeedSum = (playerSpeed p) + (playerSpeed p) * (timeForSpeed p)
    motion
      | magV (playerPos p - m) < (playerSpeed p) = (0, 0)
      | otherwise = (mulSV (60 * dt * (playerSpeedSum)) (cos $ playerDirection p, sin $ playerDirection p))

updatePlayer :: Float -> Player -> Player
updatePlayer dt p = p { playerParts = map (updatePlayerParts dt (playerTarget p)) (playerParts p)
                      , playerCenterMass = centerMass (playerParts p)
                      , timeFromSplit = max 0 ((timeFromSplit p) - dt)}