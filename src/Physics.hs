module Physics where

import Model
import Config
import System.Random
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Vector

centerMass :: [PlayerPart] -> Point
centerMass xs = sum (map g xs)
  where
    m = sum (map playerMass xs)
    g x = mulSV (playerMass x / m) (playerPos x)

radiusfromMass :: Float -> Float
radiusfromMass m = sqrt (m / pi)

speedFromMass :: Float -> Float
speedFromMass m = startSpeed * sqrt (startMass / m)

checkBoard :: Float -> Float -> Float
checkBoard x y
  | x < 0 = max x (-y)
  | otherwise = min x y

checkBoards :: Point -> Point
checkBoards point = (checkBoard (fst point) screenRight, checkBoard (snd point) screenUp)

playerPartEaten :: [Eat] -> PlayerPart -> PlayerPart
playerPartEaten e p = p 
  { playerMass =  newMass
  , playerRadius = radiusfromMass newMass
  , playerSpeed = speedFromMass newMass
  }
  where
    newMass = (playerMass p) + foldl (\x y -> x + (if (eatenByPlayerPart y p) then (eatMass y) else 0)) 0 e

playerEaten :: Player -> [Eat] -> Player
playerEaten p e = p {playerParts = map (playerPartEaten e) (playerParts p)}

playersEaten :: [Player] -> [Eat] -> [Player]
playersEaten [] _ = []
playersEaten (p:ps) e = playerEaten p e : playersEaten ps e

eatenByPlayerPart :: Eat -> PlayerPart -> Bool
eatenByPlayerPart e p = forEat (playerPos p) (eatPos e) (playerRadius p)

eatenByPlayer :: Eat -> Player -> Bool
eatenByPlayer e p = any (eatenByPlayerPart e) (playerParts p)
    
eatenByPlayers :: [Player] -> Eat -> Bool
eatenByPlayers p e = any (eatenByPlayer e) p

wasEaten :: World -> World
wasEaten w = w 
  { eat = newE 
  , players = newP
  }
  where
    newE = filter (not . eatenByPlayers (players w)) (eat w)
    newP = playersEaten (players w) (eat w)

checkTwoParts :: PlayerPart -> PlayerPart -> PlayerPart
checkTwoParts p1 p2
  | forEatPlayerPart p1 p2 = p1 {playerMass = newMass, playerRadius = newRadius, playerSpeed = newSpeed}
  | otherwise = p1
  where
    newMass = (playerMass p1) + (playerMass p2)
    newRadius = radiusfromMass newMass
    newSpeed = speedFromMass newMass

checkPlayersPart :: [PlayerPart] -> PlayerPart -> PlayerPart
checkPlayersPart ps p =  foldl checkTwoParts p ps

filterPlayersParts :: [PlayerPart] -> [PlayerPart] -> [PlayerPart]
filterPlayersParts ps1 [] = ps1
filterPlayersParts ps1 (p2 : ps2) = filterPlayersParts (filter (not . (forEatPlayerPart p2)) ps1) ps2

checkPlayersParts :: [PlayerPart] -> [PlayerPart] -> [PlayerPart]
checkPlayersParts ps1 ps2 = map (checkPlayersPart ps2) (filterPlayersParts ps1 ps2)

checkTwoPlayer :: Player -> Player -> Player
checkTwoPlayer p1 p2
  | playerID p1 == playerID p2 = p1
  | otherwise = p1{playerParts = checkPlayersParts (playerParts p1) (playerParts p2)}

checkPlayer :: [Player] -> Player -> Player
checkPlayer ps p = foldl checkTwoPlayer p ps

randomPoints :: StdGen -> [Point]
randomPoints g = zip (randomRs eatRangeX g1) (randomRs eatRangeY g2)
  where
    (g1, g2) = split g

forEat :: Point -> Point -> Float -> Bool
forEat p1 p2 r = (magV (p1 - p2)) < r

forEatPlayerPart :: PlayerPart -> PlayerPart -> Bool
forEatPlayerPart p1 p2 = forEat (playerPos p1) (playerPos p2) (playerRadius p1) && ((playerMass p1) / (playerMass p2) >= 1.25)