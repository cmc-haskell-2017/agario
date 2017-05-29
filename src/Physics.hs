module Physics where

import Model
import Config
import System.Random
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Vector

-- | Нахождение центра масс
centerMass :: [PlayerPart] -> Point
centerMass xs = sum (map g xs)
  where
    m = sum (map playerMass xs)
    g x = mulSV (playerMass x / m) (playerPos x)

-- | Радиус в зависимости от массы
radiusfromMass :: Float -> Float
radiusfromMass m = sqrt (m / pi)

-- | Скорость в зависимости от массы
speedFromMass :: Float -> Float
speedFromMass m = startSpeed * sqrt (startMass / m)

-- | Проверка границы
checkBorder :: Float -> Float -> Float
checkBorder x y
  | x < 0 = max x (-y)
  | otherwise = min x y

-- | Проверка границ
checkBorders :: Point -> Point
checkBorders point = (checkBorder (fst point) screenRight, checkBorder (snd point) screenUp)

-- | Поглощение еды частью молекулы
playerPartEaten :: [Eat] -> PlayerPart -> PlayerPart
playerPartEaten e p = p 
  { playerMass =  newMass
  , playerRadius = radiusfromMass newMass
  , playerSpeed = speedFromMass newMass
  }
  where
    newMass = (playerMass p) + foldl (\x y -> x + (if (eatenByPlayerPart y p) then (eatMass y) else 0)) 0 e

-- | Поглощение еды молекулой
playerEaten :: Player -> [Eat] -> Player
playerEaten p e = p {playerParts = map (playerPartEaten e) (playerParts p)}

-- | Поглощение еды молекулами
playersEaten :: [Player] -> [Eat] -> [Player]
playersEaten [] _ = []
playersEaten (p:ps) e = playerEaten p e : playersEaten ps e

-- | Проверка на поглощение еды частью молекулы
eatenByPlayerPart :: Eat -> PlayerPart -> Bool
eatenByPlayerPart e p = forEat (playerPos p) (eatPos e) (playerRadius p)

-- | Проверка на поглощение еды молекулой
eatenByPlayer :: Eat -> Player -> Bool
eatenByPlayer e p = any (eatenByPlayerPart e) (playerParts p)

-- | Проверка на поглощение еды молекулами   
eatenByPlayers :: [Player] -> Eat -> Bool
eatenByPlayers p e = any (eatenByPlayer e) p

-- | Обновление еды и игроков после поглощения
wasEaten :: World -> World
wasEaten w = w 
  { eat = newE 
  , players = newP
  }
  where
    newE = filter (not . eatenByPlayers (players w)) (eat w)
    newP = playersEaten (players w) (eat w)

-- | Поглощение части молекулы частью другой молекулы
checkTwoParts :: PlayerPart -> PlayerPart -> PlayerPart
checkTwoParts p1 p2
  | forEatPlayerPart p1 p2 = p1 {playerMass = newMass, playerRadius = newRadius, playerSpeed = newSpeed}
  | otherwise = p1
  where
    newMass = (playerMass p1) + (playerMass p2)
    newRadius = radiusfromMass newMass
    newSpeed = speedFromMass newMass

-- | Поглощение части молекулы другой молекулой
checkPlayersPart :: [PlayerPart] -> PlayerPart -> PlayerPart
checkPlayersPart ps p =  foldl checkTwoParts p ps

-- | Удаление поглощенных частей
filterPlayersParts :: [PlayerPart] -> [PlayerPart] -> [PlayerPart]
filterPlayersParts ps1 [] = ps1
filterPlayersParts ps1 (p2 : ps2) = filterPlayersParts (filter (not . (forEatPlayerPart p2)) ps1) ps2

-- | Поглощение молекулы другой молекулой
checkPlayersParts :: [PlayerPart] -> [PlayerPart] -> [PlayerPart]
checkPlayersParts ps1 ps2 = map (checkPlayersPart ps2) (filterPlayersParts ps1 ps2)

concatTwoParts :: PlayerPart -> PlayerPart -> PlayerPart
concatTwoParts p1 p2
  | mayConcat p1 p2 = p1 {playerMass = newMass, playerRadius = newRadius, playerSpeed = newSpeed}
  | otherwise = p1
  where
    newMass = (playerMass p1) + (playerMass p2)
    newRadius = radiusfromMass newMass
    newSpeed = speedFromMass newMass

concatPlayerPart :: [PlayerPart] -> PlayerPart -> PlayerPart
concatPlayerPart ps p = foldl concatTwoParts p ps

-- | Удажение поглощаемых частей
filterPlayerParts :: [PlayerPart] -> [PlayerPart] -> [PlayerPart]
filterPlayerParts ps1 [] = ps1
filterPlayerParts ps1 (p2 : ps2) = filterPlayerParts (filter (not . (mayConcat p2)) ps1) ps2
    
-- | Объединение своих частей
concatParts :: [PlayerPart] -> [PlayerPart]
concatParts ps = map (concatPlayerPart ps) (filterPlayerParts ps ps)

checkConcat :: Player -> Player
checkConcat p
  |(timeFromSplit p) /= 0 = p
  | otherwise = p {playerParts = concatParts (playerParts p)} 

-- | Поглощение молекулы молекулой
checkTwoPlayer :: Player -> Player -> Player
checkTwoPlayer p1 p2
  | playerID p1 == playerID p2 = checkConcat p1
  | otherwise = p1{playerParts = checkPlayersParts (playerParts p1) (playerParts p2)}

-- | Поглощение молекулы молекулами
checkPlayer :: [Player] -> Player -> Player
checkPlayer ps p = foldl checkTwoPlayer p ps

-- | Генерация случайных точек
randomPoints :: StdGen -> [Point]
randomPoints g = zip (randomRs rangeX g1) (randomRs rangeY g2)
  where
    (g1, g2) = split g

-- | Проверка на возможность поглотить еду
forEat :: Point -> Point -> Float -> Bool
forEat p1 p2 r = (magV (p1 - p2)) < r

-- | Проверка на возможность поглотить часть молекулы
forEatPlayerPart :: PlayerPart -> PlayerPart -> Bool
forEatPlayerPart p1 p2 = forEat (playerPos p1) (playerPos p2) (playerRadius p1) && ((playerMass p1) / (playerMass p2) >= 1.25)

mayConcat :: PlayerPart -> PlayerPart -> Bool
mayConcat p1 p2 = forEat (playerPos p1) (playerPos p2) (playerRadius p1) && ((playerMass p1) > (playerMass p2) || ((playerMass p1) == (playerMass p2) && (partNum p1) < (partNum p2)))
