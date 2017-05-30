module Game where

import Graphics.Gloss.Interface.IO.Game
import Control.Concurrent.STM
import System.Random
import System.Exit
import Config
import Model
import AI
import Physics
import Player

-- | Основная функция
run :: IO ()
run = do
  g <- newStdGen
  initShared <- atomically $ newTVar (initWorld g)
  playIO display bgColor fps initShared drawShared handleShared updateShared
  where
    display = InWindow "Agar.hs" (screenWidth, screenHeight) (200, 200)
    bgColor = black
    fps = 60

    drawShared s = do
      w <- readTVarIO s
      return (drawWorld w)

    handleShared (EventKey (SpecialKey KeyEsc) Down _ _) _ = exitSuccess
    handleShared e s = atomically $ do
      w <- readTVar s
      writeTVar s (handleWorld e w)
      return s

    updateShared dt s = do
      atomically $ modifyTVar s (updateWorld dt)
      return s

emptyWorld :: StdGen -> World
emptyWorld g = World 
  { playID = 0
  , eat = []
  , players = []
  , nextEat = initEat g
  }  

initOneEat :: Point -> Eat
initOneEat p = Eat
  { eatPos = p
  , eatColor = red
  , eatMass = startEatMass
  , eatRadius = radiusfromMass startEatMass
  } 

-- | Инициализация еды
initEat :: StdGen -> [Eat]
initEat g = map initOneEat (randomPoints g)

-- | Инициализация мира
initWorld :: StdGen -> World
initWorld g = w
  { playID = 1
  , players = initPlayer 1 Handle g1 : initPlayer arrowsPlayerId Handle g1 : initPlayer 2 (Bot Hungry) g4 : initPlayer 3 (Bot Hungry) g3 : initPlayer 2 (Bot Dummy) g2 : []
  }
  where
    w = (emptyWorld g)
    (g1, g2) = split g
    (g3, g4) = split g2

-- | Отрисовка еды
drawEat :: Eat -> Picture
drawEat p = color (eatColor p) (uncurry translate (eatPos p) (circleSolid (eatRadius p)))

-- | Отрисовка мира
drawWorld :: World -> Picture
drawWorld w = pictures
  [ pictures (map drawPlayer (players w))
  , pictures (map drawEat (eat w))
  ]

-- | Движение молекул
movePlayers :: Int -> Point -> World -> World
movePlayers i m w = w 
  { players = map (\x -> if ((playerID x) == i) then (movePlayer m x) else x) (players w)}

-- | Движение молекул
movePlayersArrow :: Int -> Point -> World -> World
movePlayersArrow i p w = w 
  { players = map (\x -> if ((playerID x) == i) then (movePlayerArrow x p) else x) (players w)}

-- | Обработка деления молекул
splitPlayers :: Int -> World -> World
splitPlayers i w = w
  {players = map (\x -> if ((playerID x) == i) then (splitPlayer x) else x) (players w)}

-- | Обработка событий игрока
handleWorld :: Event -> World -> World
handleWorld (EventMotion mouse) w = movePlayers (playID w) mouse w
handleWorld (EventKey (SpecialKey KeyUp) Down _ _) w = movePlayersArrow arrowsPlayerId (0, flashSpeed) w 
handleWorld (EventKey (SpecialKey KeyDown) Down _ _) w = movePlayersArrow arrowsPlayerId (0, -flashSpeed) w 
handleWorld (EventKey (SpecialKey KeyLeft) Down _ _) w = movePlayersArrow arrowsPlayerId (-flashSpeed, 0) w 
handleWorld (EventKey (SpecialKey KeyRight) Down _ _) w = movePlayersArrow arrowsPlayerId (flashSpeed, 0) w 
handleWorld (EventKey (MouseButton LeftButton) Down _ _) w = splitPlayers (playID w) w 
handleWorld (EventKey (SpecialKey KeySpace) Down _ _) w = splitPlayers arrowsPlayerId w 
handleWorld _ w = w
 
-- | Обновление молекулы  
updatePlayers :: Float -> [Player] -> [Player]
updatePlayers dt = map (updatePlayer dt)

-- | Проверка поглощения молекулами друг друга
checkPlayers :: World -> World
checkPlayers w = w {players = map (checkPlayer (players w)) (players w)}

-- | Обновление мира
updateWorld :: Float -> World -> World
updateWorld dt w = handleBots (checkPlayers (wasEaten (w 
  { eat = newEat
  , players = updatePlayers dt $ players w
  , nextEat = drop (eatCount - length (eat w)) (nextEat w)
  })))
  where
    newEat
      | length (eat w) < eatCount
        =  (eat w) ++ take (eatCount - length (eat w)) (nextEat w)
      | otherwise = (eat w)
