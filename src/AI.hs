module AI where

import Graphics.Gloss.Interface.IO.Game
import Model
import Player
import Physics
import Config

handleBots :: World -> World
handleBots w = w {players = botActions (players w) w}

botActions :: [Player] -> World -> [Player]
botActions ps w = map (\ x -> (chooseStrategy (playerType x) x w)) ps

chooseStrategy :: PlayerType -> Player -> World -> Player
chooseStrategy Handle p _ = p
chooseStrategy (Bot Dummy) p w = dummyActions p w
-- chooseStrategy Bot Easy p w = easyActions p w

dummyActions :: Player -> World -> Player
dummyActions p w = movePlayer (eatPos ((eat w)!!10)) p

-- easyActions :: Player -> World -> Player
-- easyActions p w = movePlayer (nearestEat p w) p

-- nearestEat :: Player -> World -> Point
-- nearestEat p w = foldl (\ x y -> if (playerCenterMass p) - x > playerCenterMass p) ((playerCenterMass p) + (1500, 0)) (eat w)
