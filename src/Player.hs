module Player where

import System.Random
import Model
import Physics
import Config
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Vector

-- |Заглушка
initPlayerParts :: Point -> [PlayerPart]
initPlayerParts p = [PlayerPart { partNum = 1
                              , playerMass = startMass
                              , playerRadius = radiusfromMass startMass
                              , playerSpeed = startSpeed
                              , playerPos = p
                              , playerDirection = 0.0
                              , timeForSpeed = 0.0 }]

initPlayer :: Int -> PlayerType -> StdGen -> Player
initPlayer i pt g = Player 
  { playerID = i
  , playerColor = if pt == Handle then green else yellow
  , playerType = pt
  , playerTarget = newPoint
  , playerCenterMass = newPoint
  , playerParts = initPlayerParts newPoint
  , timeFromSplit = 0.0
  }
  where
    (g1, g2) = split g
    newPoint = (fst(randomR rangeX g1), fst(randomR rangeY g2))

-- | Отрисовка частей молекулы
drawPlayerPart :: PlayerPart -> Picture
drawPlayerPart p = uncurry translate (playerPos p) (circleSolid (playerRadius p))

-- | Отрисовка молекулы
drawPlayer :: Player -> Picture
drawPlayer p = color (playerColor p) (pictures (map drawPlayerPart (playerParts p)))

-- | Движение молекулы
movePlayer :: Point -> Player -> Player
movePlayer m p = p
  { playerTarget = newTarget
  , playerParts = map (\x -> x {playerDirection = argV (newTarget - (playerPos x))}) (playerParts p)  
  }
  where
    newTarget = checkBorders m

-- | Разделение частей молекулы
splitPlayerPart :: PlayerPart -> [PlayerPart]
splitPlayerPart p
    | ((playerMass p) > 2*startEatMass) = [ p{ partNum = newPartNum, playerMass = newMass, playerRadius = newRadius, playerSpeed = newSpeed}
                                          , p{ partNum = newPartNum + 1, playerMass = newMass, playerRadius = newRadius, timeForSpeed = 1, playerSpeed = newSpeed}]
    | otherwise = [p]
    where
      newMass = (playerMass p) / 2
      newRadius = radiusfromMass newMass
      newSpeed = speedFromMass newMass
      newPartNum = (partNum p) * 2

-- | Разделение молекулы
splitPlayer :: Player -> Player
splitPlayer p 
  | ((length (playerParts p) < 20) && (any greater (playerParts p))) = p
      { playerParts = foldl (\x y -> (x ++ splitPlayerPart y)) [] (playerParts p)
      , timeFromSplit = 10
      } 
  | otherwise = p
    where
      greater x = 2*startEatMass < (playerMass x)

-- | Обновление частей молекулы
updatePlayerParts :: Float -> Point -> PlayerPart -> PlayerPart
updatePlayerParts dt m p  = p { playerPos = (playerPos p) + motion, timeForSpeed = max 0 $ (timeForSpeed p) - dt}
  where
    playerSpeedSum = (playerSpeed p) + (playerSpeed p) * (timeForSpeed p)
    motion
      | magV (playerPos p - m) < (playerSpeed p) = (0, 0)
      | otherwise = (mulSV (60 * dt * (playerSpeedSum)) (cos $ playerDirection p, sin $ playerDirection p))

-- | Обновление молекулы
updatePlayer :: Float -> Player -> Player
updatePlayer dt p = p { playerParts = map (updatePlayerParts dt (playerTarget p)) (playerParts p)
                      , playerCenterMass = centerMass (playerParts p)
                      , timeFromSplit = max 0 ((timeFromSplit p) - dt)}