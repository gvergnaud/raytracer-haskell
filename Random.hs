module Random where

import System.Random
import Vec3

getRandomVecInUnitSphere :: IO Vec3
getRandomVecInUnitSphere = do
  x <- randomIO :: IO Float
  y <- randomIO :: IO Float
  z <- randomIO :: IO Float
  let p = vec3 2 * (Vec3 x y z) - vec3 1
  if squaredLength p >= 1
    then return p
    else getRandomVecInUnitSphere