module Config where

startMass :: Float
startMass = 300

startSpeed :: Float
startSpeed = 5

screenWidth :: Int
screenWidth = 1366

screenHeight :: Int
screenHeight = 768

-- | 
eatRangeX :: (Float, Float)
eatRangeX = (-w, w)
  where
    w = fromIntegral screenWidth / 2

eatRangeY :: (Float, Float)
eatRangeY = (-h, h)
  where
    h = fromIntegral screenHeight / 2

-- | Положение верхнего края экрана
screenUp :: Float
screenUp = fromIntegral screenHeight / 2

-- | Положение нижнего края экрана
screenDown :: Float
screenDown = - fromIntegral screenHeight / 2

-- | Положение правого края экрана
screenRight :: Float
screenRight = fromIntegral screenWidth / 2

-- | Положение левого края экрана
screenLeft :: Float
screenLeft = - fromIntegral screenWidth / 2

eatCount :: Int
eatCount = 40