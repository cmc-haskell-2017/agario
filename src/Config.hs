module Config where

-- | Стартовая масса игроков
startMass :: Float
startMass = 300

startEatMass :: Float
startEatMass = 75

-- | Стартовая скорость
startSpeed :: Float
startSpeed = 5

-- | Ширина экрана
screenWidth :: Int
screenWidth = 1366

-- | Высота экрана
screenHeight :: Int
screenHeight = 768

-- | Интервал генерации объектов по X
rangeX :: (Float, Float)
rangeX = (-w, w)
  where
    w = fromIntegral screenWidth / 2

-- | Интервал генерации объектов по Y
rangeY :: (Float, Float)
rangeY = (-h, h)
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

-- | Количество еды
eatCount :: Int
eatCount = 40
