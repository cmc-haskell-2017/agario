module AI where

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Vector
import Model
import Player
import Physics
import Config

-- | Обработка действий ботов
handleBots :: World -> World
handleBots w = w {players = botActions (players w) w}

-- | Действия бота
botActions :: [Player] -> World -> [Player]
botActions ps w = map (\ x -> (chooseStrategy (playerType x) x w)) ps

-- | Выбор стратегии в зависимости от сложности бота
chooseStrategy :: PlayerType -> Player -> World -> Player
chooseStrategy Handle p _ = p
chooseStrategy (Bot Dummy) p w = dummyActions p w
chooseStrategy (Bot Easy) p w = easyActions p w

-- | Действия глупого бота
dummyActions :: Player -> World -> Player
dummyActions p w = movePlayer (eatPos ((eat w)!!0)) p

-- | Действия простого бота
easyActions :: Player -> World -> Player
easyActions p w = movePlayer (nearestEatPlayer p w) p

nearestEatPart :: World -> PlayerPart -> Point
nearestEatPart w p = foldl (\ x y -> if (magV ((playerPos p) - (eatPos y))) - (playerRadius p) < (magV ((playerPos p) - x) - (playerRadius p)) then (eatPos y) else x ) (2000, 2000) (eat w)

nearestEatPlayer :: Player -> World -> Point
nearestEatPlayer p w = foldl (\ x y -> if magV ((playerCenterMass p) - (eatPos y)) < magV ((playerCenterMass p) - x) then (eatPos y) else x) (2000, 2000) (eat w)