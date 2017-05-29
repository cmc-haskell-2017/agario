module Model where
import Graphics.Gloss.Interface.Pure.Game

data PlayerType = Handle | Bot BotType deriving(Eq)

data Eat = Eat
  { eatPos      :: Point
  , eatColor    :: Color
  , eatMass     :: Float
  , eatRadius   :: Float
  }

data Player = Player
  { playerID          :: Int
  , playerType        :: PlayerType
  , playerColor       :: Color
  , playerTarget      :: Point
  , playerParts       :: [PlayerPart]
  , playerCenterMass  :: Point
  , timeFromSplit     :: Float
  }

data PlayerPart = PlayerPart
  { partNum         :: Int
  , playerMass      :: Float 
  , playerRadius    :: Float
  , playerSpeed     :: Float
  , playerPos       :: Point
  , playerDirection :: Float
  , timeForSpeed    :: Float
  }

data World = World
  { playID    :: Int
  , eat       :: [Eat]
  , players   :: [Player]
  , nextEat   :: [Eat]
  }

data BotType = Dummy | Hungry | Agressive deriving(Eq)