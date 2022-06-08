module Random where

import System.Random
import Vec3

getRandomVector :: IO Vec3
getRandomVector = do
  x <- randomIO
  y <- randomIO
  z <- randomIO
  pure (Vec3 x y z)

getRandomVecInUnitSphere :: IO Vec3
getRandomVecInUnitSphere = do
  x <- randomIO
  y <- randomIO
  z <- randomIO
  let p = vec3 2 * (Vec3 x y z) - vec3 1
  if squaredLength p < 1
    then pure p
    else getRandomVecInUnitSphere

getRandomInUnitDisk :: IO Vec3
getRandomInUnitDisk = do
  x <- randomIO
  y <- randomIO
  let p = vec3 2 * (Vec3 x y 0) - (Vec3 1 1 0)
  if (p `dot` p < 1)
    then pure p
    else getRandomInUnitDisk

getRandomColor :: IO Vec3
getRandomColor = do
  [x1, x2, y1, y2, z1, z2] <- sequence (fmap (\_ -> randomIO) [1 .. 6])
  pure (Vec3 (x1 * x2) (y1 * y2) (z1 * z2))

getRandomItem :: [a] -> IO a
getRandomItem xs =
  (xs !!) <$> randomRIO (0, length xs - 1)