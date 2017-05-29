module Game where

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Vector
import Control.Concurrent.STM
import System.Random
import System.Exit
import Config
import Model
import AI
import Physics
import Player

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
  , eatMass = 75.0 
  , eatRadius = radiusfromMass 75.0
  } 

initEat :: StdGen -> [Eat]
initEat g = map initOneEat (randomPoints g)

initWorld :: StdGen -> World
initWorld g = (emptyWorld g)
  { playID = 1
  , players = initPlayer : initPlayer1 : []
  }

drawEat :: Eat -> Picture
drawEat p = color (eatColor p) (uncurry translate (eatPos p) (circleSolid (eatRadius p)))

drawWorld :: World -> Picture
drawWorld w = pictures
  [ pictures (map drawPlayer (players w))
  , pictures (map drawEat (eat w))
  ]

movePlayers :: Int -> Point -> World -> World
movePlayers i m w = w 
  { players = map (\x -> if ((playerID x) == i) then (movePlayer m x) else x) (players w)}

splitPlayers :: Int -> World -> World
splitPlayers i w = w
  {players = map (\x -> if ((playerID x) == i) then (splitPlayer x) else x) (players w)}

handleWorld :: Event -> World -> World
handleWorld (EventMotion mouse) w = movePlayers (playID w) mouse w
handleWorld (EventKey (SpecialKey KeySpace) Down _ _) w = splitPlayers (playID w) w 
handleWorld _ w = w
  
updatePlayers :: Float -> [Player] -> [Player]
updatePlayers dt = map (updatePlayer dt)

checkPlayers :: World -> World
checkPlayers w = w {players = map (checkPlayer (players w)) (players w)}

updateWorld :: Float -> World -> World
updateWorld dt w = handleBots (checkPlayers (wasEaten (w 
  { eat = newEat
  , players = updatePlayers dt $ players w
  , nextEat = drop (eatCount - length (eat w)) (nextEat w)
  })))
  where
    newEat
      | length (eat w) < eatCount
        = take (eatCount - length (eat w)) (nextEat w) ++ (eat w)
      | otherwise = (eat w)
